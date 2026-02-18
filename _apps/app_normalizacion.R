library(shiny)
library(ggplot2)

# Interfaz de Usuario
ui <- fluidPage(
  
  # Activamos MathJax para fórmulas matemáticas
  withMathJax(),
  
  titlePanel("Visualización: Normal vs. Estandarizada"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Parámetros de la Normal (X)"),
      
      sliderInput("mu", 
                  "Media (mu) - Desplazamiento:", 
                  min = -30, 
                  max = 30, 
                  value = 10,
                  step = 1),
      
      sliderInput("sigma", 
                  "Desviación Estándar (sigma) - Forma:", 
                  min = 1, 
                  max = 10, 
                  value = 5,
                  step = 0.5),
      
      hr(),
      h4("Explicación"),
      p("El gráfico superior tiene ejes fijos para visualizar el efecto de los parámetros."),
      tags$ul(
        tags$li("Cambiar la **Media** desplaza la curva."),
        tags$li("Cambiar la **Desviación** aplana o eleva la curva.")
      ),
      br(),
      actionButton("new_sample", "Generar nueva muestra aleatoria", icon = icon("refresh"))
    ),
    
    mainPanel(
      # Gráficos
      plotOutput("plotOriginal", height = "300px"),
      br(),
      plotOutput("plotStandard", height = "300px"),
      
      hr(),
      
      # Sección de Tabla y Fórmula
      h3("Verificación Numérica (Muestra de 10 datos)"),
      
      fluidRow(
        column(5,
               wellPanel(
                 h4("Fórmula de Estandarización"),
                 # Renderizamos la fórmula en LaTeX
                 uiOutput("formula_latex"),
                 p("Donde:"),
                 tags$ul(
                   tags$li("X = Valor original"),
                   tags$li(uiOutput("mu_text", inline = TRUE)),
                   tags$li(uiOutput("sigma_text", inline = TRUE))
                 )
               )
        ),
        column(7,
               h5("Tabla comparativa:"),
               tableOutput("sample_table")
        )
      )
    )
  )
)

# Lógica del Servidor
server <- function(input, output) {
  
  # Gráfico 1: Distribución Original
  output$plotOriginal <- renderPlot({
    mu <- input$mu
    sigma <- input$sigma
    
    base_plot <- data.frame(x = c(-45, 45))
    
    ggplot(base_plot, aes(x = x)) +
      stat_function(fun = dnorm, args = list(mean = mu, sd = sigma), 
                    geom = "area", fill = "#69b3a2", alpha = 0.6) +
      stat_function(fun = dnorm, args = list(mean = mu, sd = sigma), 
                    size = 1, color = "darkgreen") +
      geom_vline(xintercept = mu, linetype = "dashed", color = "darkgreen") +
      annotate("text", x = mu, y = 0.02, label = paste("mu =", mu), 
               vjust = -1, color = "darkgreen", fontface = "bold") +
      # Ejes fijos
      coord_cartesian(xlim = c(-40, 40), ylim = c(0, 0.45)) +
      labs(title = paste0("Distribución Original: X ~ N(", mu, ", ", sigma, ")"),
           x = "X (Escala Original)", y = "Densidad") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"))
  })
  
  # Gráfico 2: Distribución Estandarizada
  output$plotStandard <- renderPlot({
    ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
      stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                    geom = "area", fill = "#404080", alpha = 0.6) +
      stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                    size = 1, color = "navy") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "navy") +
      coord_cartesian(xlim = c(-4, 4), ylim = c(0, 0.45)) +
      labs(title = "Distribución Estandarizada: Z ~ N(0, 1)",
           x = "Z (Puntaje Z)", y = "Densidad") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"))
  })
  
  # Generar textos dinámicos para la fórmula
  output$formula_latex <- renderUI({
    helpText(withMathJax(sprintf("$$Z = \\frac{X - \\mu}{\\sigma} = \\frac{X - %s}{%s}$$", 
                                 input$mu, input$sigma)))
  })
  
  output$mu_text <- renderUI({
    withMathJax(sprintf("$$\\mu = %s$$", input$mu))
  })
  
  output$sigma_text <- renderUI({
    withMathJax(sprintf("$$\\sigma = %s$$", input$sigma))
  })
  
  # Generación de la muestra aleatoria
  # Usamos eventReactive para que cambie si cambian los inputs O si se presiona el botón
  dataset <- reactive({
    # Dependencia del botón para regenerar
    input$new_sample
    
    # Generamos 10 datos aleatorios
    x_vals <- rnorm(10, mean = input$mu, sd = input$sigma)
    z_vals <- (x_vals - input$mu) / input$sigma
    
    data.frame(
      Original_X = x_vals,
      Calculo = paste0("(", round(x_vals, 2), " - ", input$mu, ") / ", input$sigma),
      Estandarizado_Z = z_vals
    )
  })
  
  # Renderizar la tabla
  output$sample_table <- renderTable({
    dataset()
  }, digits = 4, hover = TRUE, bordered = TRUE)
}

# Ejecutar la App
shinyApp(ui = ui, server = server)