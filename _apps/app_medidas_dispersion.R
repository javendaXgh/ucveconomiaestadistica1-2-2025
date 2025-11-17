library(plotly)
library(shiny)
library(tidyverse)
# aplicacion shiny para visualizar media, mediana, 1 desviación típica, 2 desviaciones
# típica, 3 desviaciones típicas sobre una simulación de datos de una normal.
# incluye selector de valores para mostrar o no y input de cantidad de datos a simular

ui <- fluidPage(
  titlePanel("Análisis de Medidas de Dispersión"),
  sidebarLayout(
    sidebarPanel(
      numericInput('n',
                   label='Número de datos a simular',
                   value=100,
                   min=10,
                   max=1000),
      checkboxInput('mean',
                    label= 'Mostrar media (línea azul)',
                    value= FALSE),
      checkboxInput('median',
                    label= 'Mostrar mediana (línea verde)',
                    value= FALSE),
      checkboxInput('sd1',
                    label= 'Mostrar 1 desviación típica (línea roja)',
                    value= FALSE),
      checkboxInput('sd2',
                    label= 'Mostrar 2 desviaciones típicas (línea naranja)',
                    value= FALSE),
      checkboxInput('sd3',
                    label= 'Mostrar 3 desviaciones típicas (línea amarilla)',
                    value= FALSE)
    ),
    mainPanel(
      verbatimTextOutput("valores"),
      plotlyOutput("histograma")
    )
  )
)

server <- function(input, output) {
  # Generar datos aleatorios de una normal
  datos <- reactive({
    rnorm(input$n, mean = 0, sd = 1)
  })
  
  # Calcular medidas de dispersión
  medidas <- reactive({
    data.frame(
      media = mean(datos()),
      mediana = median(datos()),
      sd1 = sd(datos()),
      sd2 = sd(datos()),
      sd3 = sd(datos())
    )
  })
  
  # Mostrar valores calculados
  output$valores <- renderPrint({
    medidas()
  })
  
  # Crear histograma interactivo
  output$histograma <- renderPlotly({
    p <- ggplot(data.frame(x = datos()), aes(x)) +
      geom_histogram(aes(y = ..density..), bins = input$n / 10, fill = "lightblue", color = "black") +
      labs(title = "Histograma de Datos Aleatorios",
           x = "Valores",
           y = "Densidad") +
      theme_minimal()
    
    if (input$mean) {
      p <- p + geom_vline(aes(xintercept = medidas()$media), color = "blue", linetype = "dashed", size = 1)
    }
    
    if (input$median) {
      p <- p + geom_vline(aes(xintercept = medidas()$mediana), color = "green", linetype = "dashed", size = 1)
    }
    
    if (input$sd1) {
      p <- p + geom_vline(aes(xintercept = medidas()$media + medidas()$sd1), color = "red", linetype = "dashed", size = 1)
      p <- p + geom_vline(aes(xintercept = medidas()$media - medidas()$sd1), color = "red", linetype = "dashed", size = 1)
    }
    
    if (input$sd2) {
      p <- p + geom_vline(aes(xintercept = medidas()$media + 2 * medidas()$sd2), color = "orange", linetype = "dashed", size = 1)
      p <- p + geom_vline(aes(xintercept = medidas()$media - 2 * medidas()$sd2), color = "orange", linetype = "dashed", size = 1)
      
    }
    
  
    
    if (input$sd3) {
      p <- p + geom_vline(aes(xintercept = medidas()$media + 3 * medidas()$sd3), color = "yellow", linetype = "dashed", size = 1)
      p <- p + geom_vline(aes(xintercept = medidas()$media - 3 * medidas()$sd3), color = "yellow", linetype = "dashed", size = 1)
    }
    
    ggplotly(p)
  })
  
}

shinyApp(ui = ui, server = server)
