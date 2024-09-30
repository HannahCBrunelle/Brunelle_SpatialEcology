#Spatial Ecology - Homework #5 
#Date: October 8, 2024
#Hannah C Brunelle
#Contact: hannahb7@umbc.edu
#Resources used: ChatGPT, GitHub Copilot, 
#Assignment solutions #2, class acitivities going over SDMs

#Load the necessary libraries for this assignment
library(geodata)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(colorRamps)
library(viridisLite)

######Number 1######
#Download the bioclim variables from worldclim with resolution of 5
bioRasts <- worldclim_global(var="bio", res=5, path=getwd())

#Plot bio_1 to have a look at the data
plot(bioRasts[[1]])

#Check the CRS of the rasters
terra::crs(bioRasts, describe=T)

#Rename the layers for convenience
names(bioRasts) <- paste0("bio", 1:19)

#####Number 2#####
#Make a raster stack containing these bioclimatic variables: `bi0 2-7`, `bio10, `bio11`,'bio15','bio18' and `bio19`.
bioRasts <- subset(bioRasts, c("bio2", "bio3", "bio4", "bio5","bio6","bio7","bio10","bio11","bio15","bio18","bio19"))

#Load the Australia shapefile
ausNZ <- st_read("/Users/hannahbrunelle/Desktop/SpatialEcology/AUS")

#Check CRS of the shapefile, differs from bioRasts!
crs(ausNZ, describe=T)

#Remove New Zealand
aus.albers <- subset(ausNZ, NAME == "Australia")

#Transform aus to CRS as the bioclim rasters
aus.wgs84 <- st_transform(aus.albers, crs = st_crs(bioRasts))

#Crop the raster stack to the outline of Australia
bioRasts.aus <- crop(bioRasts, aus.wgs84, mask = TRUE)

#Plot the cropped raster stack
plot(bioRasts.aus)

#####Number 3#####
#Use the `geodata::sp_occurrence` function to download records for the Austral grass tree
#Download the species occurrence data
grassTree <- sp_occurrence(genus = "Xanthorrhoea",
                           species = "australis",
                           download=T,
                           geo=T,
                           removeZeros = T)
#Check for records without geographic coordinates...but, should be none because geo=T argument
summary(grassTree$lon)
summary(grassTree$lat)

#Convert to sf object
grassTree.sf <- sf::st_as_sf(grassTree, coords = c("lon", "lat"))

# assign CRS information
st_crs(grassTree.sf) <- 4326

# plot the data
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
ggplot() +
  geom_sf(data = world) +
  geom_sf(data = grassTree.sf, aes(color = acceptedScientificName)) +
  theme_minimal()

#Select points that fall within the Australia polygon
grassTree.sf <- sf::st_intersection(grassTree.sf, aus.wgs84)

#Plot the new data - Still a point in the middle of Aus.
ggplot() +
  geom_sf(data = aus.wgs84) +
  geom_sf(data = grassTree.sf, aes(color = acceptedScientificName)) +
  theme_minimal()

#Find the westernmost record
west <- which.min(data.frame(st_coordinates(grassTree.sf))$X)
#Remove the westernmost record
grassTree.sf <- grassTree.sf[-west,]

#Plot the new data - It worked
ggplot() +
  geom_sf(data = aus.wgs84) +
  geom_sf(data = grassTree.sf, aes(color = acceptedScientificName)) +
  theme_minimal()

#Remove spatial duplicates then and transform the points to the same CRS as the raster
grassTree.sf.albers <- st_transform(grassTree.sf, crs = crs(aus.albers))

#Find duplicate cell numbers
dups <- duplicated(grassTree.sf.albers)
#Keep only one record per climate grid cell and remove the duplicates
grassTree.sf.albers.noDups <- grassTree.sf.albers[!dups,]

#Filter points with coordinate uncertainty less than or equal to 10,000 meters (10 km)
grassTree_uncertainty_noDups <- grassTree.sf.albers.noDups %>% 
  filter(coordinateUncertaintyInMeters <= 10000)

#Ensure the date column is in Date format
grassTree_uncertainty_noDups$dateIdentified <- as.Date(grassTree_uncertainty_noDups$dateIdentified)
#Filter out points collected before 1990
grassTree_filtered <- grassTree_uncertainty_noDups %>%
  filter(dateIdentified >= as.Date("1990-01-01"))


#Transform the CRS of the grassTree_filtered to match that of the bioRasts.aus raster
grassTree_filtered <- st_transform(grassTree_filtered, crs = st_crs(bioRasts.aus.albers))
# Verify the CRS transformation
st_crs(grassTree_filtered)
st_crs(bioRasts.aus.albers)

#Check how many observations are in grassClim
num_observations <- nrow(grassClim)
#Print the result - Observations are 963 but supposed to be ~500
print(paste("Number of observations in grassClim:", num_observations))

#Plot one of the bioclimatic rasters (for example, bio2) to make sure cleaning worked
plot(bioRasts.aus.albers[[1]], main = "GrassTree Points on Bioclimatic Raster")
#Add the transformed grassTree_filtered points
plot(st_geometry(grassTree_filtered), add = TRUE, col = "red", pch = 16, cex = 0.6)

####Question 1 : How would you expect the resolution of a raster to influence the number of spatial duplicates? 
#Answer:The resolution of a raster is the amount of detail that is captured in the raster so the lower the resolution the 
#the larger the cell size and vice a versa. This means that the lower the resolution the larger the cell size which means that points 
#there may be more spatial duplicates. On the other hand, if the resolution is higher, there will be less spatial duplicates because
#the cell sizes are smaller and the points are more likely to be in their own distinct cells. 

#####Number 4#####
#Extract bioclimatic vars at the grass tree locations
grassClim <- extract(bioRasts.aus.albers, grassTree_filtered)
#Check for NA values
summary(grassClim)
#Remove NA values
grassClim <- grassClim[complete.cases(grassClim),]
#Combine the bioclimatic data with the original occurrence data
grassTree_with_bioclim <- cbind(grassTree_filtered, grassClim)
#Check the combined data
head(grassTree_with_bioclim)
class(grassTree_with_bioclim)

#####Number 5#####
#Define the extent of the raster
raster_extent <- ext(bioRasts.aus.albers)

#Generate 10,000 random background points within the raster extent
set.seed(123)  # For reproducibility
#Extract coordinates from the extent
background_points <- st_as_sf(data.frame(
  x = runif(n_background_points, min = xmin(raster_extent), max = xmax(raster_extent)),
  y = runif(n_background_points, min = ymin(raster_extent), max = ymax(raster_extent))
), coords = c("x", "y"), crs = st_crs(grassTree_filtered))

#Check the generated background points
head(background_points)

#Extract bioclimatic variables for the background points
background_bioclim <- extract(bioRasts.aus.albers, background_points)
#Combine the background points with the extracted bioclimatic variables
background_with_bioclim <- cbind(background_points, background_bioclim)
#Check the combined data
head(background_with_bioclim)
class(background_with_bioclim)

#####Number 6#####
library(usdm)

# Convert to a data frame and keep only the bioclimatic variables
bioclim_vars <- as.data.frame(st_drop_geometry(background_with_bioclim[, grep("bio", names(background_with_bioclim))]))


# Run vifstep to identify and remove correlated variables
vif_results <- vifstep(bioclim_vars)

# Check the results
print(vif_results)

# Get the remaining variables after VIF removal
selected_vars <- vif_results@results$Variables
head(selected_vars)

