library(ggplot2)

dataTest <- read.csv('../data/sample.csv', header=TRUE)

date <- as.Date(dataTest$Start_Date_Time , "%m/%d/%Y")

library(lubridate)

#oc <- table(date)

oc <- table(year(date))

#oc <- table(date)
#oc
#oc["2017-05-05"]

#########################################################

#min.date <- as.Date(paste("2017-1-1", sep = ""))
#max.date <- as.Date(paste("2021-12-31", sep = ""))
#df <- data.frame(date = seq(min.date, max.date, by="days"), value = NA)

#min.date <- as.Date(paste(format(min(date), "%Y"),"-1-1", sep = ""))
#max.date <- as.Date(paste(format(max(date), "%Y"),"-12-31", sep = ""))
min.date <- 2017
max.date <- 2021

#min.date <- year(min(date))
#max.date <- year(max(date))

df <- data.frame(date = seq(min.date, max.date), value = c(oc["2017"], oc["2018"], oc["2019"], oc["2020"], oc["2021"]))
#typeof(df)
#df$value[match(date, df$date)] <- oc
#oc["2016"]

#df$value[2016]
df
#df
#df$year  <-  as.factor(format(df$date, "%Y"))
#df$month <- as.numeric(format(df$date, "%m"))
#df$doy   <- as.numeric(format(df$date, "%j"))
#df$dow <- as.numeric(format(df$date, "%w"))
#df$woy <- as.numeric(format(df$date, "%U")) + 1

#########################################################


#x <- df$date
#y <- rev(c("Sunday", "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
y <- df$value

#x <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
#x <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
#x <- c("2016", "2017", "2018", "2019", "2020", "2021", "2022")
x <- df$date

ggplot(df, aes(x=date, y=value)) +
  geom_line() + geom_point()


