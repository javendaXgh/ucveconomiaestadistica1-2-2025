# install.packages("pak")
# install.packages("owidapi")
library(owidapi)
library(shiny)

ui <- fluidPage(
  owid_output("co2_chart")
)

server <- function(input, output) {
  owid_server(
    "co2_chart", 
    "https://ourworldindata.org/grapher/co2-emissions-per-capita"
  )
}

shinyApp(ui = ui, server = server)