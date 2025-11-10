# aplicación shiny para simular n lanzamientos de un dado
# contiene un parámetro de entrada de lanzamientos, un parámetro para crear sesgo y un gráfico

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(shinycssloaders)
library(shinyjs)

library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  useShinyjs(),
  titlePanel("Simulación de lanzamientos de un dado"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("n_lanzamientos", "Número de lanzamientos:", value = 100, min = 1),
      sliderInput("sesgo", "Sesgo del dado:", min = 0, max = 1, value = 0.5),
      actionButton("simular", "Simular lanzamientos")
    ),
    
    mainPanel(
      plotOutput("histograma") %>% withSpinner(color="#0dc5c1"),
      verbatimTextOutput("resultado")
    )
  )
)
server <- function(input, output) {
  # Función para simular lanzamientos de un dado
  simular_lanzamientos <- function(n, sesgo) {
    # Generar una distribución de probabilidad para el dado
    probabilidad <- c(1/6 * (1 - sesgo), 1/6 * (1 - sesgo), 1/6 * (1 - sesgo), 
                      1/6 * (1 - sesgo), 1/6 * (1 - sesgo), 1/6 * (1 + 5 * sesgo))
    
    # Simular los lanzamientos
    lanzamientos <- sample(1:6, n, replace = TRUE, prob = probabilidad)
    return(lanzamientos)
  }
  
  observeEvent(input$simular, {
    # Deshabilitar el botón mientras se simula
    disable("simular")
    
    # Simular los lanzamientos
    lanzamientos <- simular_lanzamientos(input$n_lanzamientos, input$sesgo)
    
    # Crear un data frame con los resultados
    resultados <- data.frame(lanzamiento = lanzamientos)
    
    # Calcular la frecuencia de cada número
    frecuencias <- resultados %>%
      group_by(lanzamiento) %>%
      summarise(frecuencia = n()) %>%
      mutate(porcentaje = frecuencia / input$n_lanzamientos * 100)
    
    # Crear el gráfico
    output$histograma <- renderPlot({
      ggplot(frecuencias, aes(x = factor(lanzamiento), y = porcentaje)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(title = "Frecuencia de lanzamientos", x = "Número del dado", y = "Porcentaje") +
        theme_minimal()
    }) 
    
    
    # Mostrar los resultados en texto
    output$resultado <- renderPrint({
      cat("Resultados de la simulación:\n")
      print(frecuencias)
    })
    
    # Habilitar el botón nuevamente después de la simulación
    enable("simular")
  })
}
shinyApp(ui = ui, server = server)
