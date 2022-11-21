library(dplyr)
library(ggplot2)

sample_data <- read.csv("data/sample.csv")

'library(stringr)'

relevant_cols <- select(data, Incident.ID, Start_Date_Time, Victims, Crime.Name1, Crime.Name2, Crime.Name3, Place, Police.District.Name)

data_place <- relevant_cols %>% mutate(Place = ifelse(grepl("Street", Place, fixed = TRUE),
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

unique(data_place$Place)

data_place <- data_place %>% mutate(Crime.Name1 = ifelse(Crime.Name1 == "", "Other", Crime.Name1))

ggplot(data_place, aes(fill=Crime.Name1, y=Victims, x=Place)) + 
  geom_bar(position="stack", stat="identity") + coord_flip()

ggplot(data_place, aes(fill=Crime.Name1, y=Victims, x=Place)) + 
  geom_bar(position="fill", stat="identity") + coord_flip()

table(data_place$Place)

