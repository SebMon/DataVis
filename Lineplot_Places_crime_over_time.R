library(dplyr)
library(ggplot2)
library(lubridate)

relevant_cols <- select(data, Incident.ID, Start_Date_Time, Victims, Crime.Name1, Crime.Name2, Crime.Name3, Place, Police.District.Name)

sample_with_changed_Place <- relevant_cols %>% mutate(Place = ifelse(grepl("Street", Place, fixed = TRUE),
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

unique(sample_with_changed_Place$Place)

sample_with_changed_Place <- sample_with_changed_Place %>% mutate(Crime.Name1 = ifelse(Crime.Name1 == "", "Other", Crime.Name1))

'Making date object'
sample_with_changed_Place$Start_Date_Time <- as.Date(sample_with_changed_Place$Start_Date_Time, format="%m/%d/%Y %I:%M:%S %p")
sample_with_changed_Place$year <- floor_date(sample_with_changed_Place$Start_Date_Time, unit="year")
'End of making date object'

sample_with_changed_Place %>%
  group_by(Place, year) %>%
  add_count() %>%
  ungroup() %>%
  ggplot(aes(x=year, y=n, color=Place)) + 
  geom_line()

'Do not use the mutate below'
sample_with_changed_Place <- sample_with_changed_Place %>% mutate(Start_Date_Time = ifelse(
  grepl("2017", Start_Date_Time, fixed = TRUE),
  "2017",
  ifelse(
    grepl("2018", Start_Date_Time, fixed = TRUE),
    "2018",
    ifelse(
      grepl("2019", Start_Date_Time, fixed = TRUE),
      "2019",
      ifelse(
        grepl("2020", Start_Date_Time, fixed = TRUE),
        "2020",
        ifelse(
          grepl("2021", Start_Date_Time, fixed = TRUE),
          "2021",
          "Other"
        ))))))
sample_with_changed_Place <- sample_with_changed_Place %>% filter(!Start_Date_Time=="Other")

unique(sample_with_changed_Place$Start_Date_Time)
