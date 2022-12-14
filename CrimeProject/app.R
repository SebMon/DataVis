library(shiny)
library(ggplot2)
library(circular)
library(fastmap)
library(lubridate)
library(gganimate)
library(dplyr)
library(tidyverse)

# Load data
data <- read.csv('./data/temp.csv')
data <- mutate(data, Crime.Name1 = ifelse(Crime.Name1 == "", "Other", Crime.Name1))
data$Start_Date_Time_Date_Objects <- as.Date(data$Start_Date_Time, format="%m/%d/%Y %I:%M:%S %p")

population = fastmap()

# Data for Calendar diagrams
#calendar_data <- read.csv('../data/sample.csv')
#calendar_date <- as.Date(data$Start_Date_Time , "%m/%d/%Y")
calendar_date <- data$Start_Date_Time_Date_Objects

calendar_minDate <- "2017-01-01"
calendar_maxDate <- "2021-12-31"
calendar_date <- subset(calendar_date, calendar_date >= calendar_minDate & calendar_date <= calendar_maxDate)

calendar_df <- data.frame(calendar_date = seq(as.Date(calendar_minDate), as.Date(calendar_maxDate), by="days"), value = NA)
calendar_df$value <- table(calendar_date)

calendar_df$year  <-  as.factor(format(calendar_df$calendar_date, "%Y"))
calendar_df$month <- as.numeric(format(calendar_df$calendar_date, "%m"))
calendar_df$doy   <- as.numeric(format(calendar_df$calendar_date, "%j"))
calendar_df$dow <- as.numeric(format(calendar_df$calendar_date, "%w"))
calendar_df$woy <- as.numeric(format(calendar_df$calendar_date, "%U")) + 1

# Convert df$value to numeric
calendar_df$value <- as.numeric(calendar_df$value)

calendar_df$dowmapped <- ordered(calendar_df$dow, levels = 6:0)
levels(calendar_df$dowmapped) <- rev(c("Sunday", "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

temp_calendar <- filter(calendar_df, calendar_df$calendar_date > '2018-01-01')

calendar_plot <- ggplot(calendar_df, aes(woy, dowmapped, fill = value)) + 
  geom_tile(colour = "darkgrey") + 
  theme(text = element_text(size=20)) +
  facet_wrap(~year, ncol = 1) + # Facet for years
  coord_equal(xlim = c(2.5,54)) + # square tiles
  scale_x_continuous(breaks = 53/12*(1:12)-1.5, labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) + 
  scale_fill_gradientn(colours = c("#ffffff", "#fbffd6", "#FFFFBD", "#FFAE63", "#db2c2c", "#A62424"), na.value = "white",
                       name = "Amount of crime",
                       guide = guide_colorbar(
                         direction = "horizontal",
                         barheight = unit(2, units = "mm"),
                         barwidth = unit(75, units = "mm"),
                         title.position = 'top',
                         title.hjust = 0.5
                       )) +
  labs(x = NULL, 
       y = NULL, 
       title = "", 
       subtitle = "")

my.lines<-data.frame(x=numeric(), 
                     y=numeric(), 
                     xend=numeric(), 
                     yend=numeric(), 
                     year=character())

for(years in levels(calendar_df$year)){
  calendar_df.subset <- calendar_df[calendar_df$year == years,]
  
  y.start <- calendar_df.subset$dow[1]
  x.start <- calendar_df.subset$woy[1]
  
  x.top.left <- ifelse(y.start == 0, x.start - 0.5, x.start + 0.5)
  y.top.left <- 7.5
  x.top.right <- calendar_df.subset$woy[nrow(calendar_df.subset)] + 0.5
  y.top.right <- 7.5
  
  x.mid.left01 <- x.start - 0.5
  y.mid.left01 <- 7.5 - y.start
  x.mid.left02 <- x.start + 0.5
  y.mid.left02 <- 7.5 - y.start
  
  x.bottom.left <- x.start - 0.5
  y.bottom.left <- 0.5
  x.bottom.right <- ifelse(y.start == 6, calendar_df.subset$woy[nrow(calendar_df.subset)] + 0.5, calendar_df.subset$woy[nrow(calendar_df.subset)] - 0.5)
  y.bottom.right <- 0.5
  
  my.lines<-rbind(my.lines,
                  data.frame(x    = c(x.top.left, x.bottom.left, x.mid.left01, x.top.left, x.bottom.left), 
                             y    = c(y.top.left, y.bottom.left, y.mid.left01, y.top.left, y.bottom.left),
                             xend = c(x.top.right, x.bottom.right, x.mid.left02, x.mid.left02, x.mid.left01), 
                             yend = c(y.top.right, y.bottom.right, y.mid.left02, y.mid.left02, y.mid.left01), 
                             year = years))
  
  # lines to separate months
  for (j in 1:12)  {
    calendar_df.subset.month <- max(calendar_df.subset$doy[calendar_df.subset$month == j])
    x.month <- calendar_df.subset$woy[calendar_df.subset.month]
    y.month <- calendar_df.subset$dow[calendar_df.subset.month]
    
    x.top.mid <- x.month + 0.5
    y.top.mid <- 7.5
    
    x.mid.mid01 <- x.month - 0.5
    y.mid.mid01 <- 7.5 - y.month - 1
    x.mid.mid02 <- x.month + 0.5
    y.mid.mid02 <- 7.5 - y.month - 1
    
    x.bottom.mid <- ifelse(y.month == 6, x.month + 0.5, x.month - 0.5)
    y.bottom.mid <- 0.5
    
    my.lines<-rbind(my.lines,
                    data.frame(x    = c(x.top.mid, x.mid.mid01, x.mid.mid01), 
                               y    = c(y.top.mid, y.mid.mid01, y.mid.mid01),
                               xend = c(x.mid.mid02, x.mid.mid02, x.bottom.mid), 
                               yend = c(y.mid.mid02, y.mid.mid02, y.bottom.mid), 
                               year = years))
    
  }
  
}

# add lines to separate months 
calendar_plot <- calendar_plot + geom_segment(data=my.lines, aes(x,y,xend=xend, yend=yend), lineend = "square", color = "black", inherit.aes=FALSE)
############################

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

data_place$Start_Date <- as.Date(data_place$Start_Date_Time, format="%m/%d/%Y %I:%M:%S %p")
data_place$year <- floor_date(data_place$Start_Date, unit="year")


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
data_place$Hour <- as.integer(format(as.POSIXct(data_place$Start_Date_Time, format="%m/%d/%Y %I:%M:%S %p"), format="%H"))
data_place$Minute <- as.integer(format(as.POSIXct(data_place$Start_Date_Time, format="%m/%d/%Y %I:%M:%S %p"), format="%M"))
data_place$HourDec <- data_place$Hour + (data_place$Minute / 60)
data_place$TimeRad <- 2 * pi * (data_place$HourDec/24)

data_place <- subset(data_place, !is.na(data_place$TimeRad))

data_place_TOD <- sample_n(data_place, 10000)

basic_time_of_day_dens = density(data_place_TOD$TimeRad, from = 0, to = 2 * pi)

rad_time_of_day_density = circular::density.circular(circular::circular(data_place_TOD$TimeRad,
                                                                        type="angle",
                                                                        units="radians",
                                                                        rotation="clock"),
                                                     kernel = "wrappednormal",
                                                     bw = basic_time_of_day_dens$bw) 

time_of_day_density = data.frame(time = as.numeric(24*((2 * pi) + rad_time_of_day_density$x) / (2*pi)),
                                 likelyhood = rad_time_of_day_density$y/(24/(2*pi)))

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
    geom_line(size = 2) +
    theme(axis.text.x = element_text(angle=90)) +
    xlab("Month") + ylab("% of crime") + 
    scale_x_date(date_labels = "%b%Y") +
    scale_color_brewer(palette = "Paired") +
    transition_reveal(month)
  
  #anim_save("./www/crimebyTypeOverTime.gif", animate(the_gif, height = 300, width = 1400, renderer = gifski_renderer(loop = FALSE)))
  anim_save("./www/crimebyTypeOverTime.mp4", animate(the_gif, height = 300, width = 1400, renderer = av_renderer()))
}

##### Create dataset for bin2d victim plot
bin2d_vict_data <- data_place %>% mutate(Victims = as.character(ifelse(Victims>=7, "7 or more", Victims)))
sum_of_victims_in_places <- bin2d_vict_data %>% group_by(Place) %>% count() %>% spread(Place, n)

# Uncomment when needed to update, and before deployment
#create_crimetype_gif()


# Define UI for application that draws a histogram
ui <- fluidPage(
  h1("Crimes in Montgomery county"),
  tabsetPanel(
    tabPanel(
      "Places",
      h2("Which places have the highest amount of victims?"),
      p("The diagram to the left shows the amount of victims for each place. The type of crime is also displayed."),
      p("The diagram to the right shows the data normalized. This can be used to see the most common type of crime for a specific place."),
      fluidRow(
        column( 6,
                plotOutput("stackedBarChart")
        ),
        column( 6,
                plotOutput("stackedBarChartNorm")
        ),
      ),
      h3("Has the number of crimes changed over time?"),
      p("The line plot below shows the change of number of crimes for each place over time."),
      fluidRow(
        column( 12,
                plotOutput("linePlotPlaces")
        )
      ),
      h2("How are the number of victims per crime distributed across different places?"),
      fluidRow(
        column(12,
               plotOutput("VictimsPlot"))
      ),
      checkboxInput("VictimPlotNormalize", "Show as percentage of all crimes in that place", value = FALSE),
    ),
    
    tabPanel(
      "Calendar Heatmap",
      h2("How has crime rates changed over the years?"),
      h3("and are there any pattern in specific days?"),
      p("The calendar heatmap shows the amount of crime each day, from 2017 to 2021."),
      fluidRow(
        column(12,
               plotOutput("CalendarPlot",
                          width = "100%",
                          height = 800)
        ),
      )
    ),
    
    tabPanel(
      "Types of crime",
      h2("What type of crime is most prevalent over time?"),
      #img(src="crimebyTypeOverTime.gif"),
      tags$video(id="video2", type="video/mp4", src="crimebyTypeOverTime.mp4", controls="controls"),
      p('This animated diagram illustrates how crimes are distributed into the categories shown in the legend. The Y-axis is the percentage of the total amount of crimes that each crime type takes up, at any given month in the date range shown in the X-axis (I.E. at each month, the percentages adds up to 100%).')
    ),
    
    tabPanel(
      "Time of day",
      h2("At what time of the day do crimes happen?"),
      
      p("This diagram shows how crimes are distributed over the day. The filters can be used to select specific combinations of places and crime types. It is possible to overlay a density curve over the bars. It is also possible to see the overall distribution (all places and crime types) in blue, along with the distribution of crimes after applying your filters. Please note that the height of all the bars will always sum to 1. If the overall distribution is shown in the background, its bars will also sum to 1 (independently of the primary bars)."),
      
      fluidRow(
        column( 8,
                plotOutput("TimeOfDayPlot"),
        ),
        
        column( 4,
                plotOutput("TimeOfDayPlotCircular"),
        )
      ),
      fluidRow(
        column( 4,
                checkboxGroupInput("TODPlaces", "Places", sort(unique(data_place$Place)), selected=sort(unique(data_place$Place))),
                
                actionLink("selectAllPlaces", "Select All"),
                actionLink("unselectAllPlaces", "Unselect All")
        ),
        column( 4,
                checkboxGroupInput("TODCrimeTypes", "Crime Types", sort(unique(data_place$Crime.Name1)), selected=sort(unique(data_place$Crime.Name1))),
                actionLink("selectAllCrimeTypes", "Select All"),
                actionLink("unselectAllCrimeTypes", "Unselect All"),
        ),
        column( 4,
                checkboxInput("TODShadow", "Show overall distribution in background", value = FALSE),
                checkboxInput("TODDensity", "Show density curve", value = FALSE)
        )
      ),
    ),
    tabPanel(
      "Report",
      h2("The groups report over crimes in Montgomery County"),
      fluidRow(
        column(12,
               p("Download the report"),
               downloadButton(
                 "downloadReport",
                 label = "Download",
                 class = NULL,
                 icon = shiny::icon("download")
               ))
      ),
    ))
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  TODData <- reactive({
    return(
      data_place_TOD %>%
        filter(Place %in% input$TODPlaces) %>%
        filter(Crime.Name1 %in% input$TODCrimeTypes)
    )
  })
  
  TODDensity <- reactive({
    local_data_place <- TODData()
    if (dim(local_data_place)[1] == 0) {
      to_return = data.frame(matrix(ncol = 2, nrow = 0))
      colnames(to_return) <- c("time", "likelyhood")
      return(to_return)
    }
    local_data_place$Hour <- as.integer(format(as.POSIXct(local_data_place$Start_Date_Time, format="%m/%d/%Y %I:%M:%S %p"), format="%H"))
    local_data_place$Minute <- as.integer(format(as.POSIXct(local_data_place$Start_Date_Time, format="%m/%d/%Y %I:%M:%S %p"), format="%M"))
    local_data_place$HourDec <- local_data_place$Hour + (local_data_place$Minute / 60)
    local_data_place$TimeRad <- 2 * pi * (local_data_place$HourDec/24)
    
    local_data_place <- subset(local_data_place, !is.na(local_data_place$TimeRad))
    
    local_basic_time_of_day_dens = density(local_data_place$TimeRad, from = 0, to = 2 * pi)
    
    local_rad_time_of_day_density = circular::density.circular(circular::circular(local_data_place$TimeRad,
                                                                                  type="angle",
                                                                                  units="radians",
                                                                                  rotation="clock"),
                                                               kernel = "wrappednormal",
                                                               bw = local_basic_time_of_day_dens$bw) 
    
    local_time_of_day_density = data.frame(time = as.numeric(24*((2 * pi) + local_rad_time_of_day_density$x) / (2*pi)),
                                           likelyhood = local_rad_time_of_day_density$y/(24/(2*pi)))
    
    return(local_time_of_day_density)
  })
  
  observe({
    if(input$selectAllPlaces == 0) return(NULL)
    else {
      updateCheckboxGroupInput(session, "TODPlaces", "Places", sort(unique(data_place$Place)), selected=sort(unique(data_place$Place)))
    }
  })
  
  observe({
    if(input$unselectAllPlaces == 0) return(NULL)
    else {
      updateCheckboxGroupInput(session, "TODPlaces", "Places", sort(unique(data_place$Place)))
    }
  })
  
  observe({
    if(input$selectAllCrimeTypes == 0) return(NULL)
    else {
      updateCheckboxGroupInput(session, "TODCrimeTypes", "Crime Types", sort(unique(data_place$Crime.Name1)), selected=sort(unique(data_place$Crime.Name1)))
    }
  })
  
  observe({
    if(input$unselectAllCrimeTypes == 0) return(NULL)
    else {
      updateCheckboxGroupInput(session, "TODCrimeTypes", "Crime Types", sort(unique(data_place$Crime.Name1)))
    }
  })
  
  TODPlot <- reactive({
    plot <- ggplot(mapping=aes(x=Hour))
    plot <- plot +
      geom_bar(TODData(), mapping = aes(y = (..count..)/sum(..count..)), 
               position = position_nudge(x = 0.5), 
               fill="gray", 
               colour="black",
               width=1)
    if (input$TODShadow) {
      plot <- plot +
        geom_bar(data_place_TOD, mapping = aes(y = (..count..)/sum(..count..)), 
                 position = position_nudge(x = 0.5), 
                 fill="blue", 
                 alpha=0.1,
                 width=1)
    }
    plot <- plot +
      scale_y_continuous(name="Frequency/Density") +
      scale_x_continuous(name="Hour",
                         breaks=c(0, 3, 6, 9, 12, 15, 18, 21, 24))
    if (input$TODDensity) {
      plot <- plot +
        geom_line(TODDensity(), 
                  mapping = aes(x=time, y=likelyhood), 
                  color="red")
    }
    if (input$TODShadow && input$TODDensity) {
      plot <- plot +
        geom_line(time_of_day_density, 
                  mapping = aes(x=time, y=likelyhood), 
                  color="blue",
                  alpha=0.25)
    }
    return(plot)
  })
  
  RoundTODPlot <- reactive({
    plot <- ggplot(mapping=aes(x=Hour)) +
      coord_polar()
    plot <- plot +
      geom_bar(TODData(), mapping = aes(y = (..count..)/sum(..count..)), 
               position = position_nudge(x = 0.5), 
               fill="gray", 
               colour="black",
               width=1)
    if (input$TODShadow) {
      plot <- plot +
        geom_bar(data_place_TOD, mapping = aes(y = (..count..)/sum(..count..)), 
                 position = position_nudge(x = 0.5), 
                 fill="blue", 
                 alpha=0.1,
                 width=1)
    }
    plot <- plot +
      scale_y_continuous(name="Frequency/Density") +
      scale_x_continuous(name="Hour",
                         breaks=c(0, 3, 6, 9, 12, 15, 18, 21, 24))
    if (input$TODDensity) {
      plot <- plot +
        geom_line(TODDensity(), 
                  mapping = aes(x=time, y=likelyhood), 
                  color="red")
    }
    if (input$TODShadow && input$TODDensity) {
      plot <- plot +
        geom_line(time_of_day_density, 
                  mapping = aes(x=time, y=likelyhood), 
                  color="blue",
                  alpha=0.25)
    }
    return(plot)
  })
  
  output$TimeOfDayPlot <- renderPlot(TODPlot())
  
  output$TimeOfDayPlotCircular <- renderPlot(RoundTODPlot())
  
  # Colors used are taken from here to ensure color blind people can differentiate between them: https://davidmathlogic.com/colorblind/#%23648FFF-%23785EF0-%23DC267F-%23FE6100-%23FFB000
  
  output$stackedBarChart <- renderPlot(
    ggplot(data_place, aes(fill=Crime.Name1, y=Place, x=Victims)) + 
      geom_bar(position="stack", stat="identity") +
      scale_fill_manual(name = "Crime type", values=c("#648FFF",
                                 "#FE6100",
                                 "#785EF0",
                                 "#DC267F",
                                 "#FFB000"))
  ) 
  
  output$stackedBarChartNorm <- renderPlot(
    ggplot(data_place, aes(fill=Crime.Name1, y=Place, x=Victims)) + 
      geom_bar(position="fill", stat="identity") +
      scale_fill_manual(name = "Crime type", values=c("#648FFF",
                                 "#FE6100",
                                 "#785EF0",
                                 "#DC267F",
                                 "#FFB000"))
  )
  
  output$linePlotPlaces <- renderPlot(
    data_place %>%
      group_by(Place, year) %>%
      add_count() %>%
      ungroup() %>%
      filter(!(data_place$year == "2016-01-01" | data_place$year == "2022-01-01")) %>%
      ggplot(aes(x=year, y=n, color=Place)) +
      ylab("Number of crimes") +
      xlab("Year") +
      geom_line() +
      geom_point()
  )
  
  VictimsPlot <- 
    reactive({
      if (input$VictimPlotNormalize){
        return(ggplot(bin2d_vict_data) +
                 geom_bin_2d(mapping = aes(x=Victims, y=Place, fill=100*..count../as.integer(sum_of_victims_in_places[y]))) +
                 stat_bin_2d(geom="text", mapping = aes(x=Victims, y=Place, label = round(100*..count../as.integer(sum_of_victims_in_places[y]), digits = 3))) +
                 scale_fill_continuous(high = "#19547b", low = "#ffd89b", trans="log2", name="Percentage", limits=c(0.001, 100)) +
                 xlab("Number of victims"))
      } else {
        return(ggplot(bin2d_vict_data) +
                 geom_bin_2d(mapping = aes(x=Victims, y=Place, fill=..count..)) +
                 stat_bin_2d(geom="text", mapping = aes(x=Victims, y=Place, label = ..count..)) +
                 scale_fill_continuous(high = "#19547b", low = "#ffd89b", trans="log2", name="Number of crimes") +
                 xlab("Number of victims"))
      }
    })
  
  output$VictimsPlot <- renderPlot(VictimsPlot())
  
  output$CalendarPlot <- renderPlot(calendar_plot)
  
  output$downloadReport <- downloadHandler(
    filename = "Visualizing_Crimes_in_Montgomery_County.pdf",
    content = function(file) {
      file.copy("www/The_Report.pdf", file)
    },
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
