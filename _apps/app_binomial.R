library(shiny)
library(ggplot2)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .well { background-color: #f7f7f7; }
      .explanation-box { 
        padding: 15px; 
        border-left: 5px solid #2c3e50; 
        background-color: #e8f0fe; 
        margin-top: 20px;
      }
      .highlight-box {
        background-color: #fff3cd;
        border: 1px solid #ffeeba;
        padding: 10px;
        border-radius: 5px;
        margin-bottom: 15px;
      }
    "))
  ),
  
  titlePanel("Distribución Binomial: Explorando n, p y k"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Parámetros Generales"),
      sliderInput("n", "Número de ensayos (n):", min = 5, max = 2500, value = 10, step = 5),
      sliderInput("p", "Probabilidad de éxito (p):", min = 0.01, max = 0.99, value = 0.99, step = 0.01),
      
      hr(),
      
      h4("Analizar un valor específico (k)"),
      div(class="highlight-box",
          sliderInput("k_sel", 
                      "Selecciona k éxitos:", 
                      min = 0, 
                      max = 255, 
                      value = 123, 
                      step = 1),
          strong(textOutput("prob_k_text"))
      ),
      
      hr(),
      checkboxInput("show_mean", "Mostrar Media", TRUE),
      checkboxInput("auto_zoom", "Zoom automático", TRUE)
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      
      fluidRow(
        column(4, wellPanel(h5("Media (E[X])"), h3(textOutput("mean_val")))),
        column(4, wellPanel(h5("Varianza"), h3(textOutput("var_val")))),
        column(4, wellPanel(h5("Sesgo"), h4(textOutput("skew_desc"), style = "color: #d35400;")))
      ),
      
      div(class = "explanation-box",
          h4("Interpretación:"),
          uiOutput("dynamic_explanation")
      )
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    updateSliderInput(session, "k_sel", max = input$n)
  })
  
  binom_data <- reactive({
    n <- input$n
    p <- input$p
    sd_val <- sqrt(n * p * (1 - p))
    mean_val <- n * p
    
    low <- max(0, floor(mean_val - 6 * sd_val))
    high <- min(n, ceiling(mean_val + 6 * sd_val))
    
    k_selected <- input$k_sel
    
    # Creamos un rango que incluya la campana Y el valor seleccionado
    # Si k está muy lejos, rellenamos el hueco con NA o simplemente unimos los vectores
    rango_campana <- low:high
    rango_total <- unique(sort(c(rango_campana, k_selected)))
    
    data.frame(
      k = rango_total,
      prob = dbinom(rango_total, size = n, prob = p),
      is_selected = (rango_total == k_selected)
    )
  })
  
  output$distPlot <- renderPlot({
    df <- binom_data()
    media <- input$n * input$p
    
    cols <- ifelse(df$is_selected, "#E74C3C", "#3498DB")
    
    # Base del gráfico
    p_plot <- ggplot(df, aes(x = k, y = prob)) +
      geom_col(fill = cols, width = 0.8) + # Quitamos border black para que se vea mejor en n grande
      labs(x = "Número de Éxitos (k)", y = "Probabilidad",
           title = paste("P(X =", input$k_sel, ") marcado en rojo")) +
      theme_minimal(base_size = 14)
    
    if (input$auto_zoom) {
      zoom_min <- min(df$k[df$prob > 1e-10]) # Filtramos probabilidades minúsculas para el zoom
      zoom_max <- max(df$k[df$prob > 1e-10])
      
      # Si no hay datos visibles (prob muy baja), usamos la media
      if(is.infinite(zoom_min)) zoom_min <- media - 5
      if(is.infinite(zoom_max)) zoom_max <- media + 5
      
      zoom_min <- min(zoom_min, input$k_sel)
      zoom_max <- max(zoom_max, input$k_sel)
      
      p_plot <- p_plot + scale_x_continuous(limits = c(zoom_min - 2, zoom_max + 2))
    }
    
    if (input$show_mean) {
      p_plot <- p_plot + geom_vline(xintercept = media, linetype = "dashed", color = "navy")
    }
    
    p_plot
  })
  
  output$mean_val <- renderText({ round(input$n * input$p, 2) })
  output$var_val <- renderText({ round(input$n * input$p * (1 - input$p), 2) })
  
  output$skew_desc <- renderText({
    if (abs(input$p - 0.5) < 0.01) "Simétrica" 
    else if (input$p < 0.5) "Sesgo Positivo (Derecha)" 
    else "Sesgo Negativo (Izquierda)"
  })
  
  # --- LOGICA CORREGIDA DE PROBABILIDAD ---
  output$prob_k_text <- renderText({
    prob <- dbinom(input$k_sel, input$n, input$p)
    
    if (prob < 1e-4) {
      # Notación científica para números muy pequeños
      paste0("P(X=", input$k_sel, ") ≈ ", format(prob, scientific = TRUE, digits = 3))
    } else {
      # Formato normal para números legibles
      paste0("P(X=", input$k_sel, ") = ", format(prob, scientific = FALSE, digits = 4))
    }
  })
  
  output$dynamic_explanation <- renderUI({
    k <- input$k_sel
    media <- input$n * input$p
    prob <- dbinom(k, input$n, input$p)
    
    # Lógica para el texto de probabilidad
    prob_text <- ""
    if (prob < 1e-9) {
      prob_text <- "<b>virtualmente 0</b> (es un evento extremadamente improbable)"
    } else if (prob < 0.001) {
      prob_text <- paste("<b>", format(prob, scientific = TRUE, digits = 3), "</b> (muy baja)")
    } else {
      prob_text <- paste("<b>", round(prob*100, 2), "%</b>")
    }
    
    distancia <- ""
    diff <- k - media
    if (abs(diff) < 0.1) distancia <- "es exactamente la media."
    else if (diff < 0) distancia <- "está muy por debajo de la media esperada."
    else distancia <- "está muy por encima de la media esperada."
    
    HTML(paste0("Has seleccionado <b>k = ", k, "</b> éxitos.<br>",
                "La probabilidad de obtener este resultado es ", prob_text, ".<br>",
                "Dado que esperábamos unos ", round(media, 1), " éxitos, tu valor ", distancia))
  })
}

shinyApp(ui = ui, server = server)