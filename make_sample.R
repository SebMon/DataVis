library(dplyr)

# Remember to open the project before running this

data <- read.csv('CrimeProject/data/Crime.csv')

relevant_cols <- select(data, Start_Date_Time, Victims, Crime.Name1, Place)

#sample_data <- sample_n(relevant_cols, 1000)

write.csv(relevant_cols, 'CrimeProject/data/temp.csv')

