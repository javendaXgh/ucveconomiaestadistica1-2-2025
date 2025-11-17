library(gapminder)
library(dplyr)
library(ggplot2)
library(shiny)
library(shinythemes)
 # CREAR APLICACION PARA VER lifeExp de Gapminder
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Gapminder Life Expectancy Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("continent", "Select Continent:",
                  choices = unique(gapminder$continent),
                  selected = "Europe"),
      sliderInput("year", "Select Year:",
                  min = min(gapminder$year),
                  max = max(gapminder$year),
                  value = min(gapminder$year),
                  step = 5,
                  sep = "")
    ),
    
    mainPanel(
      plotOutput("lifeExpPlot"),
      tableOutput("lifeExpTable")
    )
  )
  
)
server <- function(input, output) {
  
  filteredData <- reactive({
    gapminder %>%
      filter(continent == input$continent, year == input$year)
  })
  
  output$lifeExpPlot <- renderPlot({
    ggplot(filteredData(), aes(x = country, y = lifeExp, fill = country)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = paste("Life Expectancy in", input$continent, "in", input$year),
           x = "Country",
           y = "Life Expectancy") +
      theme_minimal()
  })
  
  output$lifeExpTable <- renderTable({
    filteredData() %>%
      select(country, lifeExp) %>%
      arrange(desc(lifeExp))
  })
}

shinyApp(ui = ui, server = server)
