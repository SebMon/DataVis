library(shiny)
library(ggplot2)
library(lubridate)

dataTest <- read.csv('../data/sample.csv', header=TRUE)

date <- as.Date(dataTest$Start_Date_Time , "%m/%d/%Y")
minDate <- "2017-01-01"
maxDate <- "2021-12-31"

# Set date range to be from 2017 to 2021 since they contain full data
date <- subset(date, date >= minDate & date <= maxDate)

#date_2016 <- subset(date, date >= "2016-01-01" & date <= "2016-12-31")
date_2017 <- subset(date, date >= "2017-01-01" & date <= "2017-12-31")
date_2018 <- subset(date, date >= "2018-01-01" & date <= "2018-12-31")
date_2019 <- subset(date, date >= "2019-01-01" & date <= "2019-12-31")
date_2020 <- subset(date, date >= "2020-01-01" & date <= "2020-12-31")
date_2021 <- subset(date, date >= "2021-01-01" & date <= "2021-12-31")
#date_2022 <- subset(date, date >= "2022-01-01" & date <= "2022-12-31")

#oc_2016 <- table(month(date_2016))
oc_2017 <- table(month(date_2017))
oc_2018 <- table(month(date_2018))
oc_2019 <- table(month(date_2019))
oc_2020 <- table(month(date_2020))
oc_2021 <- table(month(date_2021))
#oc_2022 <- table(month(date_2022))

m = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

df <- data.frame(year = c(rep("2017", 12), rep("2018", 12), rep("2019", 12), rep("2020", 12), rep("2021", 12)), value = c(oc_2017, oc_2018, oc_2019, oc_2020, oc_2021), months = m, grp=c(rep(1, 12), rep(2, 12), rep(3, 12), rep(4, 12), rep(5, 12)))

# Visualization
ggplot(df, aes(x=factor(months, m), y = value)) + 
  geom_line(aes(color = year, group=grp)) + 
  geom_point(aes(color = year, group=grp))


ui <- fluidPage(
  titlePanel("Crime per year"),
  
  h4(""),
  
  plotOutput("main_plot"),
  
  sliderInput("YearSlider", "Select Years", 2017, 2021, c(2017,2021), step = 1, round = TRUE, ticks = FALSE, sep = ""),
)

server <- function(input, output) {
  output$main_plot <- renderPlot({
    minDate <- input$YearSlider[1]
    maxDate <- input$YearSlider[2]
    
    df_sorted <- subset(df, year >= minDate & year <= maxDate)
    
    ggplot(df_sorted, aes(x=factor(months, m), y = value)) + 
      geom_line(aes(color = year, group=grp)) + 
      geom_point(aes(color = year, group=grp)) +
      xlab("Months") + ylab("Amount of crime")
  })
}

shinyApp(ui = ui, server = server)  

