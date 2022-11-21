library(shiny)
library(ggplot2)
library(circular)
library(fastmap)
library(lubridate)
library(gganimate)
library(dplyr)

# Load data
data <- read.csv('./data/Crime.csv')
data <- mutate(data, Crime.Name1 = ifelse(Crime.Name1 == "", "Other", Crime.Name1))
data$Start_Date_Time_Date_Objects <- as.Date(data$Start_Date_Time, format="%m/%d/%Y %I:%M:%S %p")

population = fastmap()

# Data for Place diagrams
data_place <- data
data_place <- data_place %>% mutate(Place = ifelse(grepl("Street", Place, fixed = TRUE),
"Street",
ifelse(grepl("Residence", Place, fixed = TRUE), 
       "Residence",
       ifelse(grepl("Retail", Place, fixed = TRUE),
              "Retail (shops)", 
              ifelse((Place == "Grocery/Supermarket")
                     | (Place == "Gas Station"),
                     "Gas station", 
                     ifelse(grepl("Parking", Place, fixed = TRUE),
                            "Parking lot/garage",
                            ifelse(grepl("School", Place, fixed = TRUE),
                                   "School/University/College",
                                   ifelse(grepl("Government", Place, fixed = TRUE),
                                          "Government building",
                                          ifelse(grepl("Bank", Place, fixed = TRUE),
                                                 "Bank",
                                                 ifelse(grepl("Commercial", Place, fixed = TRUE)
                                                        | grepl("Restaurant", Place, fixed = TRUE)
                                                        | grepl("Bar", Place, fixed = TRUE)
                                                        | grepl("Hotel/Motel", Place, fixed = TRUE),
                                                        "Commercial",
                                                        ifelse(grepl("Store", Place, fixed = TRUE),
                                                               "Store",
                                                               "Other"
)))))))))))

data_place$Start_Date_Time <- as.Date(data_place$Start_Date_Time, format="%m/%d/%Y %I:%M:%S %p")
data_place$year <- floor_date(data_place$Start_Date_Time, unit="year")


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

data <- subset(data, !is.na(data$TimeRad))

basic_time_of_day_dens = density(data$TimeRad, from = 0, to = 2 * pi)

rad_time_of_day_density = circular::density.circular(circular::circular(data$TimeRad,
                                                            type="angle",
                                                            units="radians",
                                                            rotation="clock"),
                                         kernel = "wrappednormal",
                                         bw = basic_time_of_day_dens$bw) 

time_of_day_density = data.frame(time = as.numeric(24*((2 * pi) + rad_time_of_day_density$x) / (2*pi)),
                          likelyhood = rad_time_of_day_density$y/(24/(2*pi))) # Gotta make sure the division is legit

create_crimetype_gif <- function() {
  # Making the date column actual date objects
  data$month <- floor_date(data$Start_Date_Time_Date_Objects, unit="month")
  
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
  
  anim_save("./www/crimebyTypeOverTime.gif", animate(the_gif, height = 300, width = 800))
}

# Uncomment when needed to update, and before deployment
# create_crimetype_gif()


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
  ),
  h2("What type of crime is most prevalent over time?"),
  fluidRow(
    img(src="crimebyTypeOverTime.gif")
  ),
  h2("What places have the highest amount of cimes?"),
  fluidRow(
    column( 6,
      plotOutput("stackedBarChart")
    ),
    column( 6,
      plotOutput("stackedBarChartNorm")
    ),
    column( 12,
      plotOutput("linePlotPlaces")
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
  
  output$stackedBarChart <- renderPlot(
    ggplot(data_place, aes(fill=Crime.Name1, y=Victims, x=Place)) + 
      geom_bar(position="stack", stat="identity") + coord_flip()
  ) 
  
  output$stackedBarChartNorm <- renderPlot(
    ggplot(data_place, aes(fill=Crime.Name1, y=Victims, x=Place)) + 
      geom_bar(position="fill", stat="identity") + coord_flip()
  )
  
  output$linePlotPlaces <- renderPlot(
    data_place %>%
      group_by(Place, year) %>%
      add_count() %>%
      ungroup() %>%
      ggplot(aes(x=year, y=n, color=Place)) + 
      geom_line()
  )
  
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)

