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
AUS <- subset(ausNZ, NAME == "Australia")

#Transform aus to CRS as the bioclim rasters
AUS <- st_transform(AUS, crs = st_crs(bioRasts))

#Crop the raster stack to the outline of Australia
bioRasts.aus <- crop(bioRasts, AUS, mask = TRUE)

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
grassTree.sf <- sf::st_intersection(grassTree.sf, AUS)

#Plot the new data - Still a point in the middle of Aus.
ggplot() +
  geom_sf(data = AUS) +
  geom_sf(data = grassTree.sf, aes(color = acceptedScientificName)) +
  theme_minimal()

#Find the westernmost record
west <- which.min(data.frame(st_coordinates(grassTree.sf))$X)
#Remove the westernmost record
grassTree.sf <- grassTree.sf[-west,]

#Plot the new data - It worked
ggplot() +
  geom_sf(data = AUS) +
  geom_sf(data = grassTree.sf, aes(color = acceptedScientificName)) +
  theme_minimal()

##### Data Cleaning Steps #####
#Filter out points collected before 1990
grassTree.sf <- grassTree.sf %>%
  filter(eventDate >= as.Date("1990-01-01"))

#Filter points with coordinate uncertainty less than or equal to 10,000 meters (10 km)
grassTree.sf <- grassTree.sf %>% 
  filter(coordinateUncertaintyInMeters < 10000)

#Transform the CRS of the grassTree_filtered to match that of the bioRasts.aus raster
grassTree.sf <- st_transform(grassTree.sf, crs = st_crs(bioRasts.aus))
# Verify the CRS transformation
crs(grassTree.sf)
crs(bioRasts.aus)
# check dimensions of bradypus
dim(grassTree.sf)
# Next, we want to extract the environmental values at each
# of the occurrence locations.
grassTree.sf <- terra::extract(bioRasts.aus, grassTree.sf, cells=TRUE, xy=TRUE) %>%
  # remove spatial duplicates
  distinct(cell, .keep_all = TRUE)
head(grassTree.sf)
dim(grassTree.sf)

#Check how many observations are in grassTree_filtered 
num_observations <- nrow(grassTree.sf)
#Print the result - Observations are 535 
print(paste("Number of observations in grassClim:", num_observations))

####Question 1 : How would you expect the resolution of a raster to influence the number of spatial duplicates? 
#Answer:The resolution of a raster is the amount of detail that is captured in the raster so the lower the resolution the 
#the larger the cell size and vice a versa. This means that the lower the resolution the larger the cell size which means that points 
#there may be more spatial duplicates. On the other hand, if the resolution is higher, there will be less spatial duplicates because
#the cell sizes are smaller and the points are more likely to be in their own distinct cells. 

#####Number 4#####
# Ensure the object is an sf object, assuming 'lon' and 'lat' are the coordinate columns
if (!inherits(grassTree.sf, "sf")) {
  grassTree.sf <- st_as_sf(grassTree.sf, coords = c("x", "y"), crs = 4326)
}

# Now extract the coordinates
coords <- sf::st_coordinates(grassTree.sf)

# Convert to a data frame with x and y columns
coords_df <- data.frame(x = coords[, 1], y = coords[, 2])

# Perform the extraction with terra
grassClim <- terra::extract(bioRasts.aus, coords_df)

#Check for NA values
summary(grassClim)

#Remove NA values
grassClim <- grassClim[complete.cases(grassClim),]
#Combine the bioclimatic data with the original occurrence data
grassTree_with_bioclim <- cbind(grassTree.sf, grassClim)
#Check the combined data
head(grassTree_with_bioclim)
class(grassTree_with_bioclim)

#####Number 5#####
## ---- Create background (pseudo-absence) data --------------------------------
# Generate 10,000 random background points within Australia
set.seed(123) # For reproducibility
background_points <- terra::spatSample(bioRasts.aus, size = 10000, method = "random", xy = TRUE, as.df = TRUE, na.rm = TRUE)
class(background_points)

# Convert background points to sf object with appropriate CRS
background_points.sf <- st_as_sf(background_points, coords = c("x", "y"), crs = st_crs(bioRasts.aus))
st_crs(background_points.sf) <- 4326
crs(background_points.sf)

# Extract the coordinates from the geometry column of the sf object
background_coords <- sf::st_coordinates(background_points.sf)

# Extract the bioclimatic values at each of the background point locations
background_bioclim <- terra::extract(bioRasts.aus, background_coords)

#Combine the occurrence data (grassTree_with_bioclim) with the background points
SDM_data <- dplyr::bind_rows(grassTree_with_bioclim, background_bioclim)

# Check the combined dataset
head(SDM_data)
dim(SDM_data)

#####Number 6#####
library(usdm)

# Convert to a data frame and keep only the bioclimatic variables
# Select only the bioclimatic variables from the combined background data
bioclim_vars <- st_drop_geometry(SDM_data) %>%
  dplyr::select(starts_with("bio")) 
# Check the structure to ensure only numeric bioclimatic variables are selected
str(bioclim_vars)
class(bioclim_vars)

# Variance Inflation Factor (VIF)
# Variable selection using VIF
usdm::vifstep(bioclim_vars)
# Variable selection using correlation and VIF
usdm::vifcor(bioclim_vars, th=0.8) 
# Remove problematic variables
remVars <- vifstep(bioclim_vars)@excluded
SDMbioclim <- bioclim_vars[,-which(names(bioclim_vars) %in% remVars)]

#####Number 7#####
set.seed(896)

# Extract coordinates (as a matrix) from the sf object
xyPres <- st_coordinates(grassTree.sf)
# Extract background point coordinates
xyBg <- sf::st_coordinates(background_points.sf)

# k-fold cross-validation partitioning
bv.kfold <- ENMeval::get.randomkfold(occs=xyPres,
                                     bg=xyBg,
                                     k=5)
# look at the structure, etc
str(bv.kfold)
table(bv.kfold$occs.grp)

# plot the data
evalplot.grps(pts=xyPres, pts.grp=bv.kfold$occs.grp, envs=stack(bioRasts))

#spatial block partition: checkerboard
bv.spBlock <- get.checkerboard1(occs=xyPres, envs=stack(bioRasts), 
                       bg=xyBg, 
                         aggregation.factor=100)

# Check results and visualize the block partitions
evalplot.grps(pts=xyPres, pts.grp=bv.spBlock$occs.grp, envs=stack(bioRasts))

# make life a little easier
selTrainTest <- as.numeric(unlist(bv.kfold))

# create a training dataset
sdmData.train <- subset(SDMbioclim, selTrainTest != 1)
dim(sdmData.train)# Fit Mahalanobis model

# create a testing dataset
sdmData.test <- subset(SDMbioclim, selTrainTest <= 1)
dim(sdmData.test)

#####There were no geometry points or "x and y" columns so I added the geometry back in using SDM_data
# Get row numbers for training and testing sets
train_rows <- which(selTrainTest != 1)
test_rows <- which(selTrainTest == 1)

# Re-add the geometry to the training set
sdmData.train.sf <- SDM_data[train_rows, ]
str(sdmData.train.sf) #Check the structure of the training set
class(sdmData.train.sf) #Check the class of the training set
# Re-add the geometry to the testing set
sdmData.test.sf <- SDM_data[test_rows, ]
str(sdmData.train.sf) #Check the structure of the training set
class(sdmData.train.sf) #Check the class of the training set

# Plot the training and testing data to ensure it worked
plot(st_geometry(sdmData.train.sf), col = "blue")
plot(st_geometry(sdmData.test.sf), add = TRUE, col = "red")

######Number 8######
# Create the base map with the raster layer
# Plot the raster layer first (bio2)
plot(bioRasts.aus$bio2, main = "Training and Testing Points on Bioclimatic Raster")
# Add background points in green
plot(st_geometry(background_points.sf), col = rgb(0, 1, 0, alpha = 0.3), add = TRUE, pch = 24, cex = 0.5)
# Add training points in blue
plot(st_geometry(sdmData.train.sf), col = "blue", add = TRUE, pch = 21, cex = 1.5)
# Add testing points in red
plot(st_geometry(sdmData.test.sf), col = "red", add = TRUE, pch = 22, cex = 1.5)
# Add a legend outside the plot area
legend(x = 115, y = -10,  # Move it outside the plot
       legend = c("Training", "Testing", "Background"), 
       col = c("blue", "red", rgb(0, 1, 0, alpha = 0.3)),  # Make sure the col vector is complete
       pch = c(21, 22, 24),  # Include the pch for the Background
       pt.cex = 1.5, 
       bty = "o", 
       cex = 1.2)

######Number 9######
?mahal

#Confused on what i am doing wrong. I am trying to run a Mahalanobis distance analysis on the training data and the bioclimatic rasters
#I really don't know what i am doing wrong because there are no xy coordinates in my 

# terra to raster
biorast.mm <- raster(bioRasts)
class(biorast.mm)

bioRasts.nobiome <- subset(biorast.mm, "biome", negate = TRUE)
#Error: Error: [subset] invalid name(s)

#Run a Mahalanobis distance analysis on the training data and the bioclimatic rasters
mahal_model <- mahal(stack(bioRast.mm), #rasterstack
                    xyPres) # Exclude longitude and latitude columns
#Error in solve.default(var(x)): system is computationally singular: reciprocal condition number = 7.78766e-18


