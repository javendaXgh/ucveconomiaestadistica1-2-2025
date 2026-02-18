library(shiny)
library(ggplot2)

# ==============================================================================
# UI (Interfaz de Usuario)
# ==============================================================================
ui <- fluidPage(
  
  # Estilos CSS
  tags$head(
    tags$style(HTML("
      .shiny-output-error { visibility: hidden; }
      .well { background-color: #f8f9fa; border: none; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
    "))
  ),
  
  titlePanel("Simulador de Distribución Normal (Ejes Fijos)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Parámetros"),
      helpText("Mueve los sliders para ver cómo cambia la forma y posición de la curva respecto al plano."),
      
      # Slider para la Media (mu)
      sliderInput("mu", 
                  HTML("Media (&mu;) - Posición:"), 
                  min = -10, 
                  max = 10, 
                  value = 0, 
                  step = 0.5),
      
      # Slider para la Desviación Típica (sigma)
      sliderInput("sigma", 
                  HTML("Desviación Típica (&sigma;) - Forma:"), 
                  min = 0.5, 
                  max = 5, 
                  value = 1, 
                  step = 0.1),
      
      hr(),
      
      h4("Visualización"),
      checkboxGroupInput("show_elements", 
                         "Mostrar elementos:",
                         choices = list(
                           "Media (Eje de simetría)" = "mean_line",
                           "Área +/- 1 Sigma (68.2%)" = "sd1",
                           "Área +/- 2 Sigmas (95.4%)" = "sd2",
                           "Área +/- 3 Sigmas (99.7%)" = "sd3"
                         ),
                         selected = c("mean_line", "sd1"))
    ),
    
    mainPanel(
      # El gráfico
      plotOutput("distPlot", height = "500px"),
      br(),
      # Panel de texto con estadísticas
      verbatimTextOutput("stats_info")
    )
  )
)

# ==============================================================================
# SERVER (Lógica del Servidor)
# ==============================================================================
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    mu <- input$mu
    sigma <- input$sigma
    
    # Rango fijo para el gráfico (basado en los límites de los sliders)
    # Esto evita que el gráfico haga "zoom" automático
    x_limits <- c(-25, 25) 
    
    # Crear gráfico base
    p <- ggplot(data.frame(x = x_limits), aes(x = x)) +
      
      # Curva principal
      stat_function(fun = dnorm, args = list(mean = mu, sd = sigma), 
                    size = 1.2, color = "#2c3e50") +
      
      # Títulos y etiquetas
      labs(title = bquote("Efecto de " ~ mu ~ " y " ~ sigma ~ " en la Curva Normal"),
           subtitle = "Observa cómo la curva se desplaza o se aplana sin cambiar la escala",
           y = "Densidad de Probabilidad",
           x = "Valor de la Variable (X)") +
      
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray50"),
        panel.grid.minor = element_blank()
      )
    
    # --- Áreas Sombreadas (Condicionales) ---
    
    # 3 Desviaciones Estándar
    if ("sd3" %in% input$show_elements) {
      p <- p + stat_function(fun = dnorm, args = list(mean = mu, sd = sigma),
                             xlim = c(mu - 3*sigma, mu + 3*sigma),
                             geom = "area", fill = "#379634", alpha = 0.3)
    }
    
    # 2 Desviaciones Estándar
    if ("sd2" %in% input$show_elements) {
      p <- p + stat_function(fun = dnorm, args = list(mean = mu, sd = sigma),
                             xlim = c(mu - 2*sigma, mu + 2*sigma),
                             geom = "area", fill = "#3498db", alpha = 0.3)
    }
    
    # 1 Desviación Estándar
    if ("sd1" %in% input$show_elements) {
      p <- p + stat_function(fun = dnorm, args = list(mean = mu, sd = sigma),
                             xlim = c(mu - sigma, mu + sigma),
                             geom = "area", fill = "#e74c3c", alpha = 0.4)
    }
    
    # Línea de la Media
    if ("mean_line" %in% input$show_elements) {
      p <- p + geom_vline(xintercept = mu, linetype = "dashed", color = "black", size = 0.8)
    }
    
    # --- FIJACIÓN DE EJES (CLAVE PARA LA VISUALIZACIÓN) ---
    p <- p + coord_cartesian(
      xlim = c(-25, 25),  # Fijo horizontalmente
      ylim = c(0, 0.85)   # Fijo verticalmente (max altura para sigma=0.5 es aprox 0.8)
    ) +
      scale_x_continuous(breaks = seq(-25, 25, 5)) # Marcas fijas en el eje X
    
    return(p)
  })
  
  output$stats_info <- renderText({
    mu <- input$mu
    sigma <- input$sigma
    altura_pico <- dnorm(mu, mu, sigma)
    
    paste0(
      " ANÁLISIS EN TIEMPO REAL:\n",
      " ------------------------\n",
      " Posición del Centro (Media) : ", mu, "\n",
      " Dispersión de Datos (Sigma) : ", sigma, "\n",
      " Altura del Pico (Curtosis)  : ", round(altura_pico, 4), "\n",
      " \n",
      " Interpretación Visual:\n",
      " - Si aumentas Sigma: La curva se aplana (baja el pico) y se ensancha.\n",
      " - Si cambias la Media: La curva se desliza a izquierda o derecha."
    )
  })
}

shinyApp(ui, server)