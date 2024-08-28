#Spatial Ecology - Homework #1 
#Date: September 3, 2024
#Hannah C Brunelle
#Contact: hannahb7@umbc.edu
#Resources used: https://guides.library.duke.edu/r-geospatial/geocode#:~:text=Geocoding%20within%20R&text=Use%20mutate_geocode()%20to%20create,your%20address)%20as%20new%20columns, 
#ChatGPT, https://www.dataquest.io/blog/for-loop-in-r/, https://www.youtube.com/watch?v=Sxjw1h50pTo

#Install Packages
install.packages("daymetr")
install.packages("tidygeocoder")
install.packages("plyr")

#Read in needed packages
library(daymetr) #easy downloads of DayMet climate data
library(dplyr) #data manipulation
library(plyr) #data organization
library(tidygeocoder) #geocoding of addresses
library(ggplot2) #plotting
library(tidyverse)

#Getting the path
getwd()

#Creating a data frame with 2 columns (Name & Address) and 3 locations 
Locations <- data.frame(
  id = 1:3,
  Location = c("Dawson City", "Brewster General Store", "Chichén Itzá"),
  Address = c(
    "1102 Front Street, Dawson City",
    "1935 Main St, Brewster, MA 02631",
    "97751 Yucatan, Mexico"
  ),
  stringsAsFactors = FALSE
)

#Printing the data frame 
print(Locations)

#Add the long. & lat. to the data frame
Locations <- Locations %>%
  geocode(address = Address, method = 'osm')  # You can also use 'census', 'geocodio', etc.

#Printing the new data frame
print(Locations)

#Save the data frame as a csv. 
write.csv(Locations, "Locations.csv", row.names = FALSE)

#Download Climate Data from DaymetR
#Using Loop through each row of the data frame
for (i in seq_len(nrow(Locations))) {  # seq_len ensures it handles empty data frames
  # Extract the site details
  site <- Locations$Location[i]
  lat <- as.numeric(Locations$lat[i])  # Explicitly convert to numeric
  lon <- as.numeric(Locations$long[i]) # Explicitly convert to numeric
  
#Print site information for debugging
  print(paste("Processing:", site, "at", lat, lon))
  
#Download Daymet data (climate data) for the each location from 1980 to 2022
  climate_data <- download_daymet(
    lat = lat,
    lon = lon,
    start = 1980,
    end = 2022,
    silent = TRUE
  )
  
#Saving the data as seperate csv. files
  filename <- paste0("daymet_data_", site, ".csv")
  write.csv(climate_data$data, filename, row.names = FALSE)
  
#Print success message for debugging and so I know it was successful 
  print(paste("Data for", site, "saved to", filename))
}

######Importing the datasets into my Rstudio off of my desktop because they weren't showing up in my global environment
library(readr)
#Brewster
daymet_data_Brewster_General_Store <- read_csv("daymet_data_Brewster General Store.csv")
View(daymet_data_Brewster_General_Store)
#Adding a column labeled Brewster to identify the location for when they are all combined 
daymet_data_Brewster_General_Store <- daymet_data_Brewster_General_Store %>% 
  mutate(label = "Brewster")

#Mexico
daymet_data_ChichénItzá <- read_csv("daymet_data_Chichén Itzá.csv")
View(daymet_data_ChichénItzá)
#Adding a column labeled Mexico to identify the location for when they are all combined 
daymet_data_ChichénItzá <- daymet_data_ChichénItzá %>% 
  mutate(label = "Mexico")

#Canada
daymet_data_DawsonCity <- read_csv("daymet_data_Dawson City.csv")
View(daymet_data_DawsonCity)
#Adding a column labeled Brewster to identify the location for when they are all combined 
daymet_data_DawsonCity <- daymet_data_DawsonCity %>% 
  mutate(label = "Canada")

######Combining the three data sets into one
data_all <- bind_rows(daymet_data_Brewster_General_Store, daymet_data_ChichénItzá, daymet_data_DawsonCity)
#View the combined data set
View(data_all)

#####Delete all extra information (only need temp)
#Couldn't get subset to work 
data_all$prcp..mm.day. <- NULL
data_all$srad..W.m.2. <- NULL
data_all$swe..kg.m.2. <- NULL
data_all$vp..Pa. <- NULL
# View the cleaned data set
View(data_all)

####I need to aggregate by Summer rows, group by year and calculate yearly averages

