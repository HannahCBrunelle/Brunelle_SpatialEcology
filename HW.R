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

#Read in needed packages 
library(geodata)
library(raster)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(dplyr)
library(terra)

##############Number 1####################
#Get the path
getwd()

#Assign the path
download_path <- "/Users/hannahbrunelle/Desktop/SpatialEcology/climate/wc2.1_5m"

#Getting info on worldclim data
?worldclim_global

#Download the bioclimatic variables (BioClim) from WorldClim
bioclimVars <- geodata::worldclim_global(country = "AUS", res = 5, var = "bio", path="/Users/hannahbrunelle/Desktop/SpatialEcology/climate/wc2.1_5m")

# check the data
bioclimVars# class, variable measurements of precipitation and temperature 
class(bioclimVars)# plot the stack
plot(bioclimVars)

##############Number 2####################
#Load the individual bioclimatic variables
bio10 <- raster(file.path(download_path, "wc2.1_5m_bio_10.tif"))  # Mean Temperature of Warmest Quarter
bio11 <- raster(file.path(download_path, "wc2.1_5m_bio_11.tif"))  # Mean Temperature of Coldest Quarter
bio18 <- raster(file.path(download_path, "wc2.1_5m_bio_18.tif"))  # Precipitation of Warmest Quarter
bio19 <- raster(file.path(download_path, "wc2.1_5m_bio_19.tif"))  # Precipitation of Coldest Quarter

#Create a raster stack with the selected variables
bio_climVars_Stack <- stack(bio10, bio11, bio18, bio19)

#Print the plot (the whole world)
plot(bio_climVars_Stack)

#download shape of Australia
ausPoly <- st_read("/Users/hannahbrunelle/Desktop/aus")# plot the data
plot(ausPoly)

#Check the CRS for the polygon
st_crs(ausPoly)

# Filter to keep only Australia
ausPoly <- ausPoly[ausPoly$NAME == "Australia", ]
# Plot the filtered shapefile
plot(ausPoly, main="Australia Only")

##############Number 3####################
#Download occurrence records for Xanthorrhoea australis and cleaning the data
austral_grass_tree_records <- geodata::sp_occurrence(genus="Xanthorrhoea",
                                                     species= "australis",
                                                     download=T,
                                                     geo=T, #Only has records with coordinates
                                                     removeZeros = T) #Removes errors in the data

# check the data
class(austral_grass_tree_records)
# check the dimensions
dim(austral_grass_tree_records)#  
# see how many records from different datasets
table((austral_grass_tree_records$datasetName)) #where did these come from

# convert to sf object
austral_grass_tree_records.sf <- sf::st_as_sf(austral_grass_tree_records.sf, coords = c("lon", "lat"))

# Remove duplicates
austral_grass_tree_records.sf <- austral_grass_tree_records.sf[!duplicated(austral_grass_tree_records.sf),]
# Remove records with invalid years
austral_grass_tree_records.sf <- austral_grass_tree_records.sf[!is.na(austral_grass_tree_records.sf$year) & austral_grass_tree_records.sf$year > 1900 & austral_grass_tree_records.sf$year <= as.numeric(format(Sys.Date(), "%Y")), ]

# use dplyr::select to select only the columns we need
austral_grass_tree_records.sf <- austral_grass_tree_records %>%
  select(acceptedScientificName, institutionCode, year, lat, lon)

#Print the data
print(austral_grass_tree_records.sf)

# check which CRS(s) the data is in
unique(austral_grass_tree_records$geodeticDatum) #what is the CRS because we need this before we can make into a SF (spatial) 

#transform to the same CRS as the auspoly object
austral_grass_tree_records.Transformed <- st_transform(austral_grass_tree_records.sf, 9001)

#save the sf file as a shapefile - it says it was 3D so making it into 2D
st_write(austral_grass_tree_records.Transformed, "/Users/hannahbrunelle/Desktop/austral_grass_tree_records.shp")
# Convert to 2D 
austral_grass_tree_records.Transformed_2D <- st_zm(austral_grass_tree_records.Transformed, drop = TRUE)
# Write to shapefile - Have to overwrite this because it saved it beofre
st_write(austral_grass_tree_records.Transformed_2D, "/Users/hannahbrunelle/Desktop/austral_grass_tree_records.shp", append = FALSE)

##############Number 4####################
st_crs(ausPoly) #9001
st_crs(austral_grass_tree_records.Transformed) #9001
st_crs(bio_climVars_Stack) #9122

# transform the data
AusPolyProj <- st_transform(ausPoly, 4326)
st_crs(AusPolyProj) #9001 check
austral_grass_tree_records.Transformed <- st_transform(austral_grass_tree_records.sf, 4326)
st_crs(austral_grass_tree_records.Transformed) #9001 check

####Crop to Extent####
# extent using coordinates
ausExt <- c(110, 155, -45, -10)
# crop the data to the extent of Australia
ausBioclim <- terra::crop(rast(bio_climVars_Stack), ausExt)
# plot the stack
plot(ausBioclim)

#####Making sure this plot works before attempting to transform bio10 and occurrence points
plot(ausBioclim$wc2.1_5m_bio_10)
plot(st_geometry(austral_grass_tree_records.sf),col=colors, add=T)
colors <- colorRampPalette(c("blue", "red"))(length(unique(austral_grass_tree_records.sf$year)))






#9001 is the CRS for the polygon and the occurrence points
#9122 is the CRS for the bioclimatic variables
#transform the bioclimatic variables to 9001
Bio10_Transform <- ausBioclim$wc2.1_5m_bio_10
crs(Bio10_Transform)
#transfrom the raster (Bio10_Transform) to 9001
Bio10Proj <- terra::project(Bio10_Transform, "+proj=longlat +datum=WGS84")
#plot the raster
plot(Bio10Proj)
crs(Bio10Proj)

# Plot the raster and points
library(ggplot2)
ggplot() +
  # Plot the raster
  geom_tile(data = as.data.frame(Bio10Proj, xy = TRUE), aes(x = x, y = y, fill = wc2.1_5m_bio_10)) +
  # Plot the occurrence points (sf object)
  geom_sf(data = austral_grass_tree_records.Transformed, color = "blue") +
  # Plot the polygons (sf object)
  geom_sf(data = AusPolyProj, fill = NA, color = "black") +
  # Add color scale
  scale_fill_viridis_c() +
  # Set minimal theme
  theme_minimal()


