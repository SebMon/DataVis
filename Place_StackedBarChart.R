library(dplyr)
library(ggplot2)

sample_data <- read.csv("data/sample.csv")

'library(stringr)'

'Testing modified Place'
sample_with_changed_Place <- sample_data %>% mutate(Place = ifelse((Place == "Street - In vehicle")
   | (Place == "Street - Other")
   | (Place == "Street - Bus Stop")
   | (Place == "Street - Alley")
   | (Place == "Street - In vehicle")
   | (Place == "Street - Residential")
   | (Place == "Street - Commercial"),
   "Street",
   
   ifelse((Place == "Residence - Other")
      | (Place == "Residence - Single Family")
      | (Place == "Residence - Apt Ofc/Storage")
      | (Place == "Residence - Driveway" )
      | (Place == "Residence - Nursing Home")
      | (Place == "Residence - Apartment/Condo")
      | (Place == "Residence -Townhouse/Duplex")
      | (Place == "Residence - Yard"), 
      "Residence",
      
      ifelse((Place == "Retail - Other")
        | (Place == "Retail - Drug Store/Pharmacy")
        | (Place == "Retail - Mall")
        | (Place == "Retail - Beauty/Barber Shop")
        | (Place == "Retail - Sporting Goods")
        | (Place == "Retail - Appliances/Electronics")
        | (Place == "Retail - Clothing")
        | (Place == "Retail - Hardware")
        | (Place == "Retail - Department/Discount Store"),
        "Retail (shops)", 
        ifelse((Place == "Grocery/Supermarket")
           | (Place == "Gas Station")
           | (Place == "Convenience Store")
           | (Place == "Liquor Store - Beer & Wine")
           | (Place == "Liquor Store - County"),
           "Store & Gas station", 
           ifelse((Place == "Parking Lot - Residential")
              | (Place == "Parking Garage - Commercial")
              | (Place == "Parking Lot - Metro")
              | (Place == "Parking Lot - Commercial")
              | (Place == "Parking Lot - Other")
              | (Place == "Parking Garage - Other")
              | (Place == "Parking Garage - County")
              | (Place == "Parking Garage - Residential")
              | (Place == "Parking Lot - Rec Center")
              | (Place == "Parking Lot - Park & Ride")
              | (Place == "Parking Lot - County"),
              "Parking lot/garage",
              ifelse((Place == "School/College - DO NOT USE")
                | (Place == "School - Elementary/Secondary"),
                "School/College",
                "Other"
)))))))

sample_with_changed_Place <- sample_with_changed_Place %>% mutate(Crime.Name1 = ifelse(Crime.Name1 == "", "Other", Crime.Name1))

ggplot(sample_with_changed_Place, aes(fill=Crime.Name1, y=Victims, x=Place)) + 
  geom_bar(position="fill", stat="identity")

ggplot(sample_with_changed_Place, aes(fill=Crime.Name1, y=Victims, x=Place)) + 
  geom_bar(position="stack", stat="identity")


'Below is optimized code'

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

ggplot(sample_with_changed_Place, aes(fill=Crime.Name1, y=Victims, x=Place)) + 
  geom_bar(position="stack", stat="identity") + coord_flip()

ggplot(sample_with_changed_Place, aes(fill=Crime.Name1, y=Victims, x=Place)) + 
  geom_bar(position="fill", stat="identity") + coord_flip()

table(sample_with_changed_Place$Place)

