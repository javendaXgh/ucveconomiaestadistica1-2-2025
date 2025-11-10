library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("darkly"),
  
  titlePanel("Visualización del Error Cuadrático Medio (Cuadrados de Error)"),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      numericInput("num_points", 
                   "Cantidad de Puntos:",
                   value = 10,
                   min = 5, 
                   max = 30, 
                   step = 5),
      sliderInput("slope", 
                  "Pendiente (m):", 
                  min = -1,
                  max = 5, 
                  value = .1, 
                  step = 0.01),
      sliderInput("intercept", 
                  "Intersección (b):",
                  min = -15, 
                  max = 40, 
                  value = 30, 
                  step = 1),
      hr(),
      checkboxInput("show_squares",
                    "Mostrar Cuadrados de Error",
                    value = FALSE),
      # mostrar línea de regresión verdadera
      checkboxInput("show_true_line",
                    "Mostrar Línea de Regresión Verdadera",
                    value = FALSE)
    ),
    mainPanel(
      width = 10,
      # Ajustar la altura del plotOutput
      plotOutput("msePlot", 
                 height = "700px",
                 fill = T), # <--- ¡Aquí está el cambio!
      h4(textOutput("mseValue"))
    )
  )
)

server <- function(input, output) {
  
  data <- reactive({
    req(input$num_points)
    set.seed(42)
    num_pts <- input$num_points
    x_min_data <- 1
    x_max_data <- 100
    x <- seq(x_min_data, x_max_data, length.out = num_pts)
    y <- 0.5 * x + 10 + rnorm(num_pts, sd = 15)
    data.frame(x = x, y = y)
  })
  
  output$msePlot <- renderPlot({
    df <- data()
    m <- input$slope
    b <- input$intercept
    df$y_predicted <- m * df$x + b
    df$error <- df$y - df$y_predicted
    df$error_sq <- df$error^2
    
    df <- df %>%
      mutate(error_category = cut(error_sq,
                                  breaks = c(-Inf, 50, 200, 500, Inf),
                                  labels = c("Muy Bajo", "Bajo", "Medio", "Alto"),
                                  right = FALSE,
                                  include.lowest = TRUE))
    
    p <- ggplot(df, aes(x = x, y = y)) +
      geom_point(color='blue',size=5) +
      geom_abline(intercept = b,
                  slope = m,
                  color = "black",
                  linetype = "dashed",
                  linewidth=2) +
      labs(
        # title = "Datos y Línea de Predicción con Cuadrados de Error", 
           x = "X", 
           y = "Y") +
      theme_minimal() +
      geom_segment(aes(x = x,
                       xend = x,
                       y = y_predicted,
                       yend = y),
                   color = "red",
                   arrow = arrow(length = unit(0.1,"cm")),
                   # linetype = "dotted",
                   linewidth = 2)+

      # theme(
      #   plot.background = element_rect(fill = "#2E2E2E", colour = NA),
      #   panel.background = element_rect(fill = "#2E2E2E", colour = NA),
      #   axis.text = element_text(color = "white"),
      #   axis.title = element_text(color = "white"),
      #   plot.title = element_text(color = "white"),
      #   legend.text = element_text(color = "white"),
      #   legend.title = element_text(color = "white"),
      #   panel.grid.major = element_line(color = "gray40"),
      #   panel.grid.minor = element_line(color = "gray20")
      # ) +
      scale_x_continuous(limits = c(min(df$x), max(df$x)),
                         breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = max(1, round((max(x)-min(x))/10, 0)))) +
      scale_y_continuous(limits = c(min(df$y) - sd(df$y), max(df$y) + sd(df$y)),
                         breaks = function(y) seq(floor(min(y) - sd(y)), ceiling(max(y) + sd(y)), by = max(5, round((max(y) + sd(y) - (min(y) - sd(y)))/10, 0)))) +
      coord_fixed(ratio = 1)
    
    if (input$show_squares) {
      p <- p +
        geom_segment(aes(x = x, 
                         xend = x,
                         y = y_predicted,
                         yend = y), 
                     color = "gray", 
                     linetype = "dotted") +
        geom_rect(aes(xmin = x,
                      xmax = x + error,
                      ymin = y_predicted,
                      ymax = y,
                      fill = error_category),
                  alpha = 0.3, 
                  color = "darkgray", 
                  linewidth = 0.5) +
        scale_fill_manual(values = c("Muy Bajo" = "lightgreen", 
                                     "Bajo" = "yellow",
                                     "Medio" = "orange", 
                                     "Alto" = "red"),
                          name = "Magnitud del Error Cuadrático")+
        theme(legend.position = "bottom")
    }
    

    if (input$show_true_line) {
      # Línea de regresión verdadera
      p <- p +
        geom_smooth(method = "lm",
                    se = FALSE, 
                    color = "green")
    }
    p
  })
  
  output$mseValue <- renderText({
    df <- data()
    m <- input$slope
    b <- input$intercept
    df$y_predicted <- m * df$x + b
    mse <- mean((df$y - df$y_predicted)^2)
    paste("Error Cuadrático Medio (MSE):", round(mse, 2))
  })
}

shinyApp(ui = ui, server = server)

