library(shiny)

data <- read.csv('../data/sample.csv')

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("My Shiny App"),
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
}

# Run the application 
shinyApp(ui = ui, server = server)