library(shiny)
library(modelr)
library(tidyverse)

options(na.action = na.warn)

# Define UI for application that draws a ggplot_abline with
# numeric input for n of observations to graph

ui <- fluidPage(
  titlePanel("🔎 Búscar un Modelo "),
  sidebarLayout(
    sidebarPanel(
      numericInput("n", 
                   "Number of observations:", 
                   10,
                   min = 1,
                   max = 250)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic required to draw a ggplot_abline
server <- function(input, output) {
  
  n_observaciones <- reactive(input$n)
  
  models <- reactive({
    n_obs <- n_observaciones()
    
    # Generate random coefficients for the models
    tibble(
      a1 = runif(250, -20, 40),
      a2 = runif(250, -5, 5))%>%
      slice(1:input$n)
  })

    
  #   tibble(
  #     a1 = runif(250, -20, 40),
  #     a2 = runif(250, -5, 5))%>%
  #     slice(1:n_obs)
  # )
  output$plot <- renderPlot({
    req(input$n)

    df <- models()

    ggplot(sim1,aes(x, y)) + 
      geom_abline(aes(intercept = a1, 
                      slope = a2), 
                  data = df, alpha = 1/4) +
      geom_point() 
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)