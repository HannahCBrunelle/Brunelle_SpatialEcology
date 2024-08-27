#Spatial Ecology - August 27, 2024
#Hannah C Brunelle
#Contact: hannahb7@umbc.edu

#Install Packages
install.packages("daymetr")
install.packages("tidygeocoder")

#Read in needed packages
library(daymetr) #easy downloads of DayMet climate data
library(dplyr) #data manipulation
library(tidygeocoder) #geocoding of addresses
library(ggplot2) #plotting

#Creating a data frame with 2 columns (Name & Address) and 3 locations 
Locations <- data.frame(
  id = 1:3,
  Location = c("National Gallery of Denmark", "Pilgrim Monument", "Iguazu Falls"),
  Address = c(
    "Øster Voldgade 4A, 1350 København, Denmark",
    "1 High Pole Hill Rd, Provincetown, MA 02657",
    "Iguazu Falls, Misiones, Argentina"
  ),
  stringsAsFactors = FALSE
)

#Printing the data frame 
print(Locations)

#Add the long. & lat. to the data frame
LatLong_data <- Locations %>%
  geocode(address = Address, method = 'osm')  # You can also use 'census', 'geocodio', etc.

#Printing the new data frame
print(LatLong_data)

#Save the data frame as a csv. 
write.csv(LatLong_data, "LatLong_data.csv", row.names = FALSE)
