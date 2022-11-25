library(ggplot2)

dataTest <- read.csv('../data/sample.csv', header=TRUE)

date <- as.Date(dataTest$Start_Date_Time , "%m/%d/%Y")
minDate <- "2017-01-01"
maxDate <- "2021-12-31"

months = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

# Set date range to be from 2017 to 2021 since they contain full data
date <- subset(date, date >= minDate & date <= maxDate)

library(lubridate)

oc <- table(month(date))

df <- data.frame(date = months, value = c(oc[1:12]))

ggplot(df, aes(x=factor(date, level=months), y=value, group=1)) +
  geom_line()


