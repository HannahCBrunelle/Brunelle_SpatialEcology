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

#Creating a data frame with 2 columns (Location & Address) 
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

#Add the long. & lat. to the data frame using geocode
Locations <- Locations %>%
  geocode(address = Address, method = 'osm')  

#Printing the new data frame
print(Locations)

#Save the data frame as a csv. 
write.csv(Locations, "Locations.csv", row.names = FALSE)

#Download Climate Data from DaymetR
#Using Loop through each row of the data frame
for (i in seq_len(nrow(Locations))) { 
  # Extract the site details
  site <- Locations$Location[i]
  lat <- as.numeric(Locations$lat[i])  #convert to numeric
  lon <- as.numeric(Locations$long[i]) #convert to numeric
  
#Print site information for debugging
  print(paste("Processing:", site, "at", lat, lon))
  
#Download Daymet data (climate data) for the each location from 1980 to 2020
  climate_data <- download_daymet(
    lat = lat,
    lon = lon,
    start = 1980,
    end = 2020,
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
###Cleaning the data set 
daymet_data_Brewster_General_Store$prcp..mm.day. <- NULL
daymet_data_Brewster_General_Store$srad..W.m.2. <- NULL
daymet_data_Brewster_General_Store$swe..kg.m.2. <- NULL
daymet_data_Brewster_General_Store$vp..Pa. <- NULL
# View the cleaned data set
View(daymet_data_Brewster_General_Store)

#Mexico
daymet_data_ChichénItzá <- read_csv("daymet_data_Chichén Itzá.csv")
View(daymet_data_ChichénItzá)
#Adding a column labeled Mexico to identify the location for when they are all combined 
daymet_data_ChichénItzá <- daymet_data_ChichénItzá %>% 
  mutate(label = "Mexico")
###Cleaning the data set 
daymet_data_ChichénItzá$prcp..mm.day. <- NULL
daymet_data_ChichénItzá$srad..W.m.2. <- NULL
daymet_data_ChichénItzá$swe..kg.m.2. <- NULL
daymet_data_ChichénItzá$vp..Pa. <- NULL
# View the cleaned data set
View(daymet_data_ChichénItzá)

#Canada
daymet_data_DawsonCity <- read_csv("daymet_data_Dawson City.csv")
View(daymet_data_DawsonCity)
#Adding a column labeled Canada to identify the location for when they are all combined 
daymet_data_DawsonCity <- daymet_data_DawsonCity %>% 
  mutate(label = "Canada")
###Cleaning the data set 
daymet_data_DawsonCity$prcp..mm.day. <- NULL
daymet_data_DawsonCity$srad..W.m.2. <- NULL
daymet_data_DawsonCity$swe..kg.m.2. <- NULL
daymet_data_DawsonCity$vp..Pa. <- NULL
# View the cleaned data set
View(daymet_data_DawsonCity)

######Combining the three data sets into one
data_all <- bind_rows(daymet_data_Brewster_General_Store, daymet_data_ChichénItzá, daymet_data_DawsonCity)
#View the combined data set
View(data_all)

#Filter the data for summer days (day 170 to 260)
summer_data <- data_all[data_all$yday >= 170 & data_all$yday <= 260, ]
#Check to make sure it worked
view(summer_data)

#Aggregate the data to calculate the average temperature for each year at each location
average_summer_temp <- aggregate(tmax..deg.c. ~ year + label, data = summer_data, FUN = mean)
#Check to make sure it worked
view(average_summer_temp)

#Using colorblind friendly colors 
install.packages("RColorBrewer")
library(RColorBrewer)
#Looking at the color codes
display.brewer.all()

#Using ggplot (geomsmooth) to plot the summer average yearly temps 
#label is location 
#Using the data set with calculated averages
summerplot <- ggplot(data = average_summer_temp, aes(x = year, y = tmax..deg.c., color = label, group = label)) +
  geom_smooth(size = 1) +  # Adds lines to the plot
  labs(title = "Average Summer Temperature by Year and Location",
       x = "Year",
       y = "Average Temperature (°C)",
       color = "Location") +
  scale_color_brewer(palette = "PiYG") 

#Look at plot
print(summerplot)

#Save the plot
#Confirmed that the plot was saved in the appropriate file
ggsave(filename = "average_summer_temperature.png", plot = summerplot, width = 10, height = 6, dpi = 300)

#####Answers to #6 Questions
#Canada's summer averages have always been significantly colder compared to Mexico/near the equator.
#Canada's summers have seen a gradual increase in 1980 and has been at a steady temperature (approx.).
#Mexico/Southern areas saw a slight decrease in summer averages from 1990-2000 but then increased slowly and has remained steady since 2010.
#The most increase of average summer temperatures were seen in Massachusetts. There has been a steady increase since 1990. 
#In summary I would say that based off of this plot that there has been minimal change in Mexico and Canada over the last 15 years. However, Massachusetts summers have been getting hotter.
#I would calculate some statistical tests, I believe a t-test examining the statistical significance between locations would be beneficial.  

#Extra
#Filter the data for winter days (day 60 to 350)
winter_data <- data_all[data_all$yday >= 60 & data_all$yday <= 350, ]
#Check to make sure it worked
view(winter_data)

#Aggregate the data to calculate the average temperature for each year at each location
average_winter_temp <- aggregate(tmax..deg.c. ~ year + label, data = winter_data, FUN = mean)
#Check to make sure it worked
view(average_winter_temp)

#Using ggplot (geomsmooth) to plot the summer average yearly temps 
#label is location 
#Using the data set with calculated averages
winterplot <- ggplot(data = average_winter_temp, aes(x = year, y = tmax..deg.c., color = label, group = label)) +
  geom_smooth(size = 1) +  # Adds lines to the plot
  labs(title = "Average Winter Temperature by Year and Location",
       x = "Year",
       y = "Average Temperature (°C)",
       color = "Location") +
  scale_color_brewer(palette = "PiYG") 

#Look at plot
print(winterplot)

#Save the plot
#Confirmed that the plot was saved in the appropriate file
ggsave(filename = "average_winter_temperature.png", plot = winterplot, width = 10, height = 6, dpi = 300)
