library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(networkD3)
library(gganimate)

data <- read.csv('data/sample.csv')
data <- mutate(data, Crime.Name1 = if_else(Crime.Name1 == "", "Other", Crime.Name1))
data$Start_Date_Time <- as.Date(data$Start_Date_Time, format="%m/%d/%Y %I:%M:%S %p")

create_crimetype_gif <- function() {
  # Making the date column actual date objects
  data$month <- floor_date(data$Start_Date_Time, unit="month")
  
  crimeByTypeOverTime <- aggregate(data$Incident.ID, by=list(crime= data$Crime.Name1, month = data$month), length)
  months_total <- aggregate(crimeByTypeOverTime$x, by=list(month_date=crimeByTypeOverTime$month), FUN=sum)
  crimeByTypeOverTime$percentage = NA
  
  for(i in seq_len(nrow(crimeByTypeOverTime))) {
    crimeByTypeOverTime$percentage[i] <- crimeByTypeOverTime$x[i] / filter(months_total, month_date == crimeByTypeOverTime$month[i])$x * 100
  }
  
  the_gif <- ggplot(crimeByTypeOverTime, aes(x=month, y=percentage, group=crime, color=crime)) +
    geom_line() +
    theme(axis.text.x = element_text(angle=90)) +
    xlab("Month") + ylab("% of crime") + 
    scale_x_date(date_labels = "%b%Y") +
    transition_reveal(month)
  
  anim_save("./CrimeProject/www/crimebyTypeOverTime.gif", animate(the_gif, height = 300, width = 800))
}

# Uncomment when needed to update, and before deployment
#create_crimetype_gif()



# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("My Shiny App"),
  img(src="crimebyTypeOverTime.gif"),
  plotOutput("crimeTypeOverTime")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
}

# Run the application 
shinyApp(ui = ui, server = server)
