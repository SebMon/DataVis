library(ggplot2)

data <- read.csv('../data/sample.csv', header=TRUE)

date <- as.Date(data$Start_Date_Time , "%m/%d/%Y")

minDate <- "2017-01-01"
maxDate <- "2021-12-31"

# Set date range to be from 2017 to 2021 since they contain full data
date <- subset(date, date >= minDate & date <= maxDate)

df <- data.frame(date = seq(as.Date(minDate), as.Date(maxDate), by="days"), value = NA)
df$value <- table(date)

df$year  <-  as.factor(format(df$date, "%Y"))
df$month <- as.numeric(format(df$date, "%m"))
df$doy   <- as.numeric(format(df$date, "%j"))
df$dow <- as.numeric(format(df$date, "%w"))
df$woy <- as.numeric(format(df$date, "%U")) + 1

# Convert df$value to numeric
df$value <- as.numeric(df$value)

df$dowmapped <- ordered(df$dow, levels = 6:0)
levels(df$dowmapped) <- rev(c("Sunday", "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
g <- ggplot(df, aes(woy, dowmapped, fill = value)) + 
  geom_tile(colour = "darkgrey") + 
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
       title = "Crime per day", 
       subtitle = "")

my.lines<-data.frame(x=numeric(), 
                     y=numeric(), 
                     xend=numeric(), 
                     yend=numeric(), 
                     year=character())

for(years in levels(df$year)){
  df.subset <- df[df$year == years,]
  
  y.start <- df.subset$dow[1]
  x.start <- df.subset$woy[1]
  
  x.top.left <- ifelse(y.start == 0, x.start - 0.5, x.start + 0.5)
  y.top.left <- 7.5
  x.top.right <- df.subset$woy[nrow(df.subset)] + 0.5
  y.top.right <- 7.5
  
  x.mid.left01 <- x.start - 0.5
  y.mid.left01 <- 7.5 - y.start
  x.mid.left02 <- x.start + 0.5
  y.mid.left02 <- 7.5 - y.start
  
  x.bottom.left <- x.start - 0.5
  y.bottom.left <- 0.5
  x.bottom.right <- ifelse(y.start == 6, df.subset$woy[nrow(df.subset)] + 0.5, df.subset$woy[nrow(df.subset)] - 0.5)
  y.bottom.right <- 0.5
  
  my.lines<-rbind(my.lines,
                  data.frame(x    = c(x.top.left, x.bottom.left, x.mid.left01, x.top.left, x.bottom.left), 
                             y    = c(y.top.left, y.bottom.left, y.mid.left01, y.top.left, y.bottom.left),
                             xend = c(x.top.right, x.bottom.right, x.mid.left02, x.mid.left02, x.mid.left01), 
                             yend = c(y.top.right, y.bottom.right, y.mid.left02, y.mid.left02, y.mid.left01), 
                             year = years))
  
  # lines to separate months
  for (j in 1:12)  {
    df.subset.month <- max(df.subset$doy[df.subset$month == j])
    x.month <- df.subset$woy[df.subset.month]
    y.month <- df.subset$dow[df.subset.month]
    
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

# add lines
g <- g + geom_segment(data=my.lines, aes(x,y,xend=xend, yend=yend), lineend = "square", color = "black", inherit.aes=FALSE)

g

