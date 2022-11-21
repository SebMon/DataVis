library(dplyr)

# Remember to open the project before running this

data <- read.csv('./data/Crime.csv')

relevant_cols <- select(data, Incident.ID, Start_Date_Time, Victims, Crime.Name1, Crime.Name2, Crime.Name3, Place, Police.District.Name)

sample_data <- sample_n(relevant_cols, 1000)

write.csv(sample_data, './data/sample.csv')
