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

'Below does not work, it was a desperate try to remove spaghetti'
library("sjmisc")
test <- sample_data2 %>% mutate(Place = ifelse(str_contains(Place, "Street"), "Street", Place))
test$Place
