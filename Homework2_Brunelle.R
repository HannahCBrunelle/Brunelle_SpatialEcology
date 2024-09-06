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
plot(bio10)

#download shape of Australia
ausPoly <- st_read("/Users/hannahbrunelle/Desktop/aus")# plot the data
plot(ausPoly)

st_crs(ausPoly)

# Filter to keep only Australia
ausPoly <- ausPoly[ausPoly$NAME == "Australia", ]
# Plot the filtered shapefile
plot(ausPoly, main="Australia Only")

st_crs(ausPoly)

# select points that fall within the sw polygon
grass.aus <- try(sf::st_intersection(austral_grass_tree_records.sf, AusPolyProj))

# transform the data
AusPolyProj <- st_transform(ausPoly, 4326)
st_crs(AusPolyProj)

#plot the data using ggplot
ggplot() +
  geom_sf(data = AusPolyProj) +
  geom_sf(data = grass.aus, aes(color = year)) +
  theme_minimal()

# spatial extent of Australia
#ausExt <- rnaturalearth::ne_countries(scale = "medium", #returnclass = "sf") %>%
#  filter(name == "Australia")
# extent using coordinates
ausExt <- c(110, 155, -45, -10)
# crop the data to the extent of Australia
ausBioclim <- terra::crop(bio10, ausExt)
# plot the stack
plot(ausBioclim)

# mask the data to the extent of the swPoly polygon
ausBioclimMasked <- terra::crop(ausBioclim, AusPolyProj, mask=T)
ausBioclimMasked <- terra::crop(ausBioclim, AusPolyProj, mask=T)

# plot the stack
plot(ausBioclimMasked)

# extract the values for the karri records
grass.bioclim <- terra::extract(ausBioclimMasked, grass.aus, cells=TRUE)
# check the data
head(grass.bioclim)# summarize the extracted data. Any NAs?
summary(grass.bioclim)

# add the raster values to the karri.sw object
grass.aus <- cbind(grass.aus, grass.bioclim)

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

# check which CRS(s) the data is in
unique(austral_grass_tree_records$geodeticDatum) #what is the CRS because we need this before we can make into a SF (spatial) 

# use dplyr::select to select only the columns we need
austral_grass_tree_records.sf <- austral_grass_tree_records %>%
  select(acceptedScientificName, institutionCode, year, lat, lon)

# convert to sf object
austral_grass_tree_records.sf <- sf::st_as_sf(austral_grass_tree_records.sf, coords = c("lon", "lat"))

# assign CRS information
sf::st_crs(austral_grass_tree_records.sf) <- 4326 #WGS84
st_crs(austral_grass_tree_records.sf) <- 4326

#Print the data
print(austral_grass_tree_records.sf)

# remove duplicates
austral_grass_tree_records.sf <- austral_grass_tree_records.sf[!duplicated(austral_grass_tree_records.sf),]
# Remove records with invalid years
austral_grass_tree_records.sf <- austral_grass_tree_records.sf[!is.na(austral_grass_tree_records.sf$year) & austral_grass_tree_records.sf$year > 1900 & austral_grass_tree_records.sf$year <= as.numeric(format(Sys.Date(), "%Y")), ]

# plot the data
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
ggplot() +
  geom_sf(data = world) +
  geom_sf(data = austral_grass_tree_records.sf, aes(color = year)) +
  theme_minimal()


library(colorRamps)
plot(bio10Proj, col=rgb.tables(1000), alpha=0.5) # alpha  sets transparency
plot(aus, add=T) # add the polygon


#Troubleshooting because the plot was not working but the crs was not assigned
class(austral_grass_tree_records.sf)
st_geometry(austral_grass_tree_records.sf)












# Save the sf object as a shapefile # append overwrite the old shapefile
st_write(occurrence_sf, "xanthorrhoea_australis_cleaned.shp", append = FALSE)

##############Number 4####################

