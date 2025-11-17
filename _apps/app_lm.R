# aplicación shiny para mostrar resultados de un modelo de regresión lineal
# muestra datos, gráficos de línea estimada y resultados del modelo, coeficientes estimados
# gráfico de residuales. usa ggplot para graficar

library(shiny)
library(ggplot2)
library(dplyr)
library(broom)
library(shinythemes)
library(shinyWidgets)
# library(shinycssloaders)
library(shinyjs)
# library(shinyBS)
# library(shinyalert)
# library(shinyvalidate)
# library(shinyFiles)
library(DT)

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("flatly"),
  useShinyjs(),
  
  titlePanel("Modelo de Regresión Lineal"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", 
                "Cargar archivo CSV", 
                accept = c(".csv")),
      # textInput("response",
      #           "Variable dependiente (Y):"),
      virtualSelectInput(
        inputId = "response",
        label = "Variable dependiente (Y):", 
        choices = NULL,
        width = "100%",
        dropboxWrapper = "body"
      ),
      virtualSelectInput(
        inputId = "predictors",
        label = "Variables independientes (X), separadas por comas:", 
        choices = NULL,
        width = "100%",
        dropboxWrapper = "body"
      ),

      sliderInput( 
          inputId = "rango",
          label = "seleccionar rango de variables independientes (X):",
        min = 0, max = 3, 
        value = c(1, 2) 
      ),

      actionButton("run_model", "Ejecutar Modelo"),
      hr(),
      helpText("Asegúrese de que el archivo CSV tenga encabezados adecuados.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Datos",
                 plotOutput("data_in") ,
                 verbatimTextOutput('correlacion'),
                 dataTableOutput("data_table")),
                 # tableOutput("data_table")),
        tabPanel("Gráf. Hist. Variables",
                 
                 verbatimTextOutput("valores1"),
                 plotOutput('hist1', width = "60%"),
                 verbatimTextOutput("valores2"),
                 plotOutput('hist2', width = "60%"),
        ),
        tabPanel("Gráf. de Regresión",
                 plotOutput("regression_plot") ),
        tabPanel("Resultados del Modelo",
                 verbatimTextOutput("model_summary")),
        tabPanel("Gráf. de Residuales",
                 plotOutput("residuals_plot") ),
        tabPanel("Gráf. de Residuales 2",
                 plotOutput('residuales2') ,
                 plotOutput('residuales_density'))
      )
    )
  )
)
# Define server logic

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$datafile)
    read.csv(input$datafile$datapath)
  })
  
  rango2 <- reactiveVal(NULL)
  data2 <- reactiveVal(NULL)

  observeEvent(input$rango,{
    # filter in data() selected input$range
    req(input$rango)
    
    df <- data()
    df <- df %>%
      filter(df[[input$predictors]] >= input$rango[1] & 
             df[[input$predictors]] <= input$rango[2])
    
    data2(df)
    
  })
  
  observeEvent(data(),{
    # update selectInput for response variable
    updateVirtualSelect( inputId = "response", 
                         choices = names(data()),
                         selected = names(data())[1]
                      )
    updateVirtualSelect( inputId = "predictors", 
                         choices = names(data()),
                         selected = names(data())[1]
    )
  })
  
  observeEvent(input$predictors,{
    # x = input$predictors, y = input$response
    req(input$predictors)

    updateSliderInput(session,
                      inputId= "rango", 
                      min= min(data()[[input$predictors]]), 
                      max=max(data()[[input$predictors]]),
                      value = c(min(data()[[input$predictors]]), 
                                max(data()[[input$predictors]])))
    
    output$data_in <- renderPlot({
      req(data2())
      df <- data2()
      ggplot(data=df, aes_string(x=input$predictors[1],
                              y= input$response[1]))+
        geom_point(color='red', size=7)+
        theme_grey()
    })
  })
  
  model <- eventReactive(input$run_model, {
    req(input$response, input$predictors)
    df <- data2()
    response_var <- input$response
    predictors_vars <- unlist(strsplit(input$predictors, ","))
    
    formula <- as.formula(paste(response_var, "~", paste(predictors_vars, collapse = "+")))
    
    lm(formula, data = df)
    
  })
  
  # output$data_table <- renderTable({
  #   req(data())
  #   head(data())
  # })
  
  output$data_table <- renderDataTable({
    req(data2())
    datatable(data2(), 
              options = list(pageLength = 5, 
                             autoWidth = TRUE))
  })
  
  output$regression_plot <- renderPlot({
    req(model())
    df <- data2()
    ggplot(df, aes_string(x = input$predictors, y = input$response)) +
      geom_point() +
      geom_smooth(method = "lm",
                  se = FALSE, 
                  color = "blue") +
      labs(title = "Gráfico de Regresión",
           x = input$predictors,
           y = input$response)
  })
  
  output$model_summary <- renderPrint({
    req(model())
    summary(model())
  })
  
  output$residuals_plot <- renderPlot({
    req(model())
    residuals_df <- broom::augment(model())
    
    ggplot(residuals_df, aes_string(x = ".fitted",
                                    y = ".resid")) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Gráfico de Residuales", x = "Valores Ajustados", y = "Residuales")
  })
  
  # grafico que muestra lineas verticales entre la linea de predicción y los residuales
  output$residuales2 <- renderPlot({
    req(model())
    residuals_df <- broom::augment(model())
    
    ggplot(residuals_df, aes_string(x = ".fitted", y = ".resid")) +
      geom_point() +
      geom_segment(aes(xend = .fitted, yend = 0), color = "blue") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Gráfico de Residuales con Líneas Verticales", 
           x = "Valores Ajustados", y = "Residuales")
  })
  
  
  output$residuales_density <-  renderPlot({
    req(model())
    residuals_df <- broom::augment(model())
    ggplot(residuals_df, aes_string(x = ".resid")) +
      geom_density()+
      geom_vline(aes(xintercept=0), color='red')
  })
  
  # histograma variable predictora
  output$hist1 <- renderPlot({
    req(data2())
    
    ggplot(data2(), 
           aes_string(x =  input$predictors)) +
      geom_histogram(bins = 30, 
                     fill = "blue", 
                     alpha = 0.7) +
      labs(title = "Histograma Var. Predictora", 
           x = input$predictors, 
           y = "Frecuencia")
  },height = 200, width = 600 )
  
  # histograma variable respuesta
  output$hist2 <- renderPlot({
    req(data2())
    
    ggplot(data2(), 
           aes_string(x =  input$response)) +
      geom_histogram(bins = 30, 
                     fill = "blue", 
                     alpha = 0.7) +
      labs(title = "Histograma Var. Respuesta", 
           x = input$response, 
           y = "Frecuencia")
  }, height = 200, width = 600 )
  
  
  # estadisticos variable predictora
  output$valores1 <- renderPrint({
    req(data2())
    df <- data2()
    mean_val <- round(mean(df[[input$predictors]],0))
    # print(df[[input$predictors]])
    median_val <- round(median(df[[input$predictors]],0))
    sd_val <- round(sd(df[[input$predictors]]),0)
    
    paste0("Media: ",
           mean_val,
           ". Mediana: ",
           median_val,
           ". Desviación Típica:",
           sd_val)
  })
  
  # coeficiente de correlación de pearson
  output$correlacion <- renderPrint({
    req(data2())
    df <- data2()
    paste('Coeficiente de Correlación de Pearson:',
          round(cor(df[[input$predictors]], df[[input$response]], method = 'pearson'),2))
  })
  
  # estadisticos variable respueta
  output$valores2 <- renderPrint({
    req(data2())
    df <- data2()
    mean_val <- round(mean(df[[input$response]],0))
    # print(df[[input$predictors]])
    median_val <- round(median(df[[input$response]],0))
    sd_val <- round(sd(df[[input$response]]),0)
    
    paste0("Media: ",
           mean_val,
           ". Mediana: ",
           median_val,
           ". Desviación Típica:",
           sd_val)
  })
}



shinyApp(ui = ui, server = server)