#Spatial Ecology - Homework #2 
#Date: September 10, 2024
#Hannah C Brunelle
#Contact: hannahb7@umbc.edu
#Resources used: ChatGPT

#Install packages 
install.packages("geodata")
install.packages("raster")
install.packages("sf")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")

#Read in needed packages 
library(geodata)
library(terra)
library(raster)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(ggplot2)
library(tidyverse)

#Get the path
getwd()

#Assign the path
download_path <- "/Users/hannahbrunelle/Desktop/SpatialEcology/climate/wc2.1_5m"

#Getting info on worldclim data
?worldclim_global

#Specify the resolution that I want 
resolution <- 5

#Download the bioclimatic variables (BioClim) from WorldClim
wc_data <- worldclim_global(var = "bio", res = resolution, path = download_path)

#Load the individual bioclimatic variables
bio10 <- raster(file.path(download_path, "wc2.1_5m_bio_10.tif"))  # Mean Temperature of Warmest Quarter
bio11 <- raster(file.path(download_path, "wc2.1_5m_bio_11.tif"))  # Mean Temperature of Coldest Quarter
bio18 <- raster(file.path(download_path, "wc2.1_5m_bio_18.tif"))  # Precipitation of Warmest Quarter
bio19 <- raster(file.path(download_path, "wc2.1_5m_bio_19.tif"))  # Precipitation of Coldest Quarter

#Create a raster stack with the selected variables
bio_stack <- stack(bio10, bio11, bio18, bio19)

#Print the plot (the whole world)
plot(bio_stack)

#This allows me to obtain the shapefile for Australia
####I am hoping that I am doing this correct. I am still a little confused regarding the extent 
australia_shape <- ne_countries(scale = "medium", country = "Australia", returnclass = "sf")

#Transforming the shape of Australia to the same CRS as the raster stack
australia_shape <- st_transform(australia_shape, crs(bio_stack))

#Crop and mask the raster stack to Australia - masking the rest of the world
bio_stack_australia <- mask(crop(bio_stack, australia_shape), australia_shape)

#Plot the cropped raster stack
plot(bio_stack_australia)

#Download occurrence records for Xanthorrhoea australis and cleaning the data
austral_grass_tree_records <- geodata::sp_occurrence(genus="Xanthorrhoea",
                                species= "australis",
                                download=T,
                                geo=T, #Only has records with coordinates
                                removeZeros = T) #Removes errors in the data

#check the data
class(austral_grass_tree_records)   
#check the dimensions
dim(austral_grass_tree_records) 
#see how many records from different datasets
table((austral_grass_tree_records$datasetName)) #where did these come from

#check which CRS(s) the data is in
unique(austral_grass_tree_records$geodeticDatum) #what is the CRS because we need this before we can make into a SF (spatial) 
#4326 #WGS84 

# convert to sf object
austral_grass_tree_records.sf <- st_as_sf(austral_grass_tree_records, 
                                          coords = c("lon", "lat"), 
                                          crs = 4326,   # WGS84 coordinate system and assigning the crs
                                          remove = FALSE)  # Keep original coordinate columns
#Print the data
print(austral_grass_tree_records.sf)

#use dplyr::select to select only the columns we need 
#I had to include lat and lon because it needed geographic coordinates to run
austral_grass_tree_records.sf <- austral_grass_tree_records.sf %>%
  select(acceptedScientificName,institutionCode, year,lon, lat)
#Print the data
print(austral_grass_tree_records.sf)

#Save shapefile to a specific directory
st_write(austral_grass_tree_records.sf, "/Users/hannahbrunelle/Desktop/SpatialEcology/austral_grass_tree_records1.shp")


######################Help
# plot the data # this one work but I cannot get the bio10 in the background
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
ggplot() +
  geom_sf(data = AUSPoly) +
  geom_sf(data = austral_grass_tree_records.sf, aes(color = year)) +
  theme_minimal()

# polygon for Australia
AUSPoly <- australia_shape # plot the data
plot(AUSPoly)
plot(AUSPoly, add = TRUE)

# select points that fall within the AUS polygon
austral_grass_tree_records.sf <- try(sf::st_intersection(austral_grass_tree_records.sf, AUSPoly))
# determine the CRS of AUSPoly
st_crs(AUSPoly)

# transform the data
AUSPoly <- st_transform(AUSPoly, 4326)
bio10 <- st_transform(bio10, 4326)
crs(bio10) <- st_crs(4326)$proj4string
crs(bio10)

####no different color per year and no bio10 in the background
ggplot() +
  geom_sf(data = AUSPoly) +
  geom_sf(data = austral_grass_tree_records.sf, aes(color = year()) +
  theme_minimal())


