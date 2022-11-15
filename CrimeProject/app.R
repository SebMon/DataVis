library(shiny)
library(ggplot2)
library(circular)

# Load data

data <- read.csv('../data/sample.csv')

# Data taken from here: https://datacommons.org/tools/timeline#place=geoId%2F24031&statsVar=Count_Person&chart=%7B%22count%22%3A%7B%22pc%22%3Afalse%7D%7D
population$set("2016", 1039327)
population$set("2017", 1047239)
population$set("2018", 1048794)
population$set("2019", 1051129)
population$set("2020", 1051816)
population$set("2021", 1054827)

# Example to retrieve data: population$get("2020")

# Preprocessing

##### Calculate circular density for time-of-day plot
data$Hour <- as.integer(format(as.POSIXct(data$Start_Date_Time, format="%m/%d/%Y %I:%M:%S %p"), format="%H"))
data$Minute <- as.integer(format(as.POSIXct(data$Start_Date_Time, format="%m/%d/%Y %I:%M:%S %p"), format="%M"))
data$HourDec <- data$Hour + (data$Minute / 60)
data$TimeRad <- 2 * pi * (data$HourDec/24)

basic_time_of_day_dens = density(data$TimeRad, from = 0, to = 2 * pi)

rad_time_of_day_density = circular::density.circular(circular::circular(data$TimeRad,
                                                            type="angle",
                                                            units="radians",
                                                            rotation="clock"),
                                         kernel = "wrappednormal",
                                         bw = basic_time_of_day_dens$bw) 

time_of_day_density = data.frame(time = as.numeric(24*((2 * pi) + rad_density$x) / (2*pi)),
                          likelyhood = rad_density$y/(24/(2*pi))) # Gotta make sure the division is legit

# Define UI for application that draws a histogram
ui <- fluidPage(
  h1("Crimes in Montgomery county"),
  
  h2("At what time of the day do crimes happen?"),
  
  fluidRow(
    column( 8,
      plotOutput("TimeOfDayPlot"),
    ),

    column( 4,
      plotOutput("TimeOfDayPlotCircular")
    )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$TimeOfDayPlot <- renderPlot(
      ggplot(data, mapping=aes(x=Hour)) +
        #coord_polar() +
        geom_bar(aes(y = (..count..)/sum(..count..)), 
                 position = position_nudge(x = 0.5), 
                 fill="gray", 
                 colour="black",
                 width=1) +
        scale_y_continuous(name="Density") +
        scale_x_continuous(name="Hour",
                           breaks=c(0, 3, 6, 9, 12, 15, 18, 21, 24)) +
        geom_line(time_of_day_density, 
                  mapping = aes(x=time, y=likelyhood), 
                  color="red")
  )
  
  output$TimeOfDayPlotCircular <- renderPlot(
    ggplot(data, mapping=aes(x=Hour)) +
      coord_polar() +
      geom_bar(aes(y = (..count..)/sum(..count..)), 
               position = position_nudge(x = 0.5), 
               fill="gray", 
               colour="black",
               width=1) +
      scale_y_continuous(name="Density") +
      scale_x_continuous(name="Hour",
                         breaks=c(0, 3, 6, 9, 12, 15, 18, 21, 24)) +
      geom_line(time_of_day_density, 
                mapping = aes(x=time, y=likelyhood), 
                color="red")
  )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
