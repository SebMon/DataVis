library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)

data <- read.csv('data/sample.csv')

# Making the date column actual date objects
data$start_date <- as.Date(data$Start_Date_Time, format="%m/%d/%Y %I:%M:%S %p", tz="EST")

# Adding a column with string representation of month and year. Example: "jan 2022"
data$year_month <- format(data$start_date, format="%b %Y")

# Creating vector of all months between first date and last date. This is done in string format and date format (date has to have day, month year, cant just be date and year)
allMonths <- c(format(seq.Date(min(data$start_date), max(data$start_date), by ="month"), "%b %Y"))
monthAsDate <- seq.Date(min(data$start_date), max(data$start_date), by ="month")

# Initializing vectors to be put into new data frame
society <- double(length(allMonths))
person <- double(length(allMonths))
property <- double(length(allMonths))
notCrime <- double(length(allMonths))
other <- double(length(allMonths))

# Calculating the percentage of crime type committed in each month
for (i in 1:length(allMonths)) {
  crimesTotal <- nrow(data[data$year_month == allMonths[i], ])
  society[i] <- nrow(data[data$year_month == allMonths[i] & data$Crime.Name1 == "Crime Against Society", ]) / crimesTotal * 100
  person[i] <- nrow(data[data$year_month == allMonths[i] & data$Crime.Name1 == "Crime Against Person", ]) / crimesTotal * 100
  property[i] <- nrow(data[data$year_month == allMonths[i] & data$Crime.Name1 == "Crime Against Property", ]) / crimesTotal * 100
  notCrime[i] <- nrow(data[data$year_month == allMonths[i] & data$Crime.Name1 == "Not a crime", ]) / crimesTotal * 100
  other[i] <- (nrow(data[data$year_month == allMonths[i] & data$Crime.Name1 == "Other", ]) + nrow(data[data$year_month == month & data$Crime.Name1 == "", ]))  / crimesTotal * 100
}

# Creating new data frame with data gathered from above
crimeTypePerMonth <- data.frame(
  month = allMonths,
  society = society,
  person = person,
  property = property,
  notCrime = notCrime,
  other = other,
  monthAsDate = monthAsDate
)

# Making the dataframe ordered by the month as it is, not alphabetical
crimeTypePerMonth$month <- factor(crimeTypePerMonth$month, levels = crimeTypePerMonth$month)


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("crimeTypeDateRange", "Date Range:",
                     start = min(crimeTypePerMonth$monthAsDate) - 31,
                     end = max(crimeTypePerMonth$monthAsDate) + 31)
    ),
    mainPanel(
      plotOutput("crimeTypeOverTime")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$crimeTypeOverTime <- renderPlot({
    dataToVis <- filter(crimeTypePerMonth, monthAsDate >= input$crimeTypeDateRange[1] & monthAsDate <= input$crimeTypeDateRange[2])
    
    ggplot(dataToVis, aes(x=month, group=1)) +
      geom_line(aes(y=society, color="Crime against society")) +
      geom_line(aes(y=person, color="Crime against person")) +
      geom_line(aes(y=property, color="Crime against property")) +
      geom_line(aes(y=notCrime, color="Not a crime")) +
      geom_line(aes(y=other, color="Other")) +
      theme(axis.text.x = element_text(angle=90)) +
      scale_x_discrete(breaks = dataToVis$month[
        seq(1, length(dataToVis$month), by = (length(dataToVis$month)/15))]) +
      xlab("Month") + ylab("% of crime")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
