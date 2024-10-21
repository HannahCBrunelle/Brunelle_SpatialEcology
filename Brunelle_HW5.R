#Spatial Ecology - Homework #5 
#Date: October 8, 2024
#Hannah C Brunelle
#Contact: hannahb7@umbc.edu
#Resources used: ChatGPT, GitHub Copilot, 
#Assignment solutions #2, class acitivities going over SDMs, Brian T and office hours

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

#assign CRS information
st_crs(grassTree.sf) <- 4326

#Plot the data
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

#Check dimensions
dim(grassTree.sf)
class(grassTree.sf)

#Extract the environmental values at each of the occurrence locations.
grassTree.sf <- terra::extract(bioRasts.aus, grassTree.sf, cells=TRUE, xy=TRUE) %>%
  #remove spatial duplicates
  distinct(cell, .keep_all = TRUE)
head(grassTree.sf)
dim(grassTree.sf)
class(grassTree.sf)

#Check how many observations are in grassTree 
num_observations <- nrow(grassTree.sf)
#Print the result - Observations are 535 
print(paste("Number of observations in grassClim:", num_observations))

####Question 1 : How would you expect the resolution of a raster to influence the number of spatial duplicates? 
#Answer:The resolution of a raster is the amount of detail that is captured in the raster so the lower the resolution the 
#larger the cell size and vice a versa. This means that the lower the resolution the larger the cell size, which means that there may be more 
#spatial duplicates. On the other hand, if the resolution is higher, there will be less spatial duplicates because
#the cell sizes are smaller and the points are more likely to be in their own distinct cells. 

#####Number 4#####
#Extract only the x and y columns from grassTree.sf
coords <- grassTree.sf[, c("x", "y")]

#Extract bioclimatic variable values at each occurrence point
grassClim <- terra::extract(bioRasts.aus, coords, cells=TRUE, xy=TRUE) %>%
  # Remove spatial duplicates based on raster cell ID
  distinct(cell, .keep_all = TRUE)

#Preview the data
head(grassClim)
dim(grassClim)

#Count how many records are in the final dataset
num_observations <- nrow(grassClim)
print(paste("Number of observations in grassTree_with_bioclim:", num_observations))

#####Number 5#####
# Generate 10,000 random background points within Australia
set.seed(123) # For reproducibility
background_points <- terra::spatSample(bioRasts.aus, size = 10000, method = "random", xy = TRUE, as.df = TRUE, na.rm = TRUE)
class(background_points)

#Preview the background points
head(background_points)
dim(background_points)

#Extract the bioclimatic variables for the 10,000 background points
background_bioclim <- terra::extract(bioRasts.aus, background_points[, c("x", "y")], cells=TRUE, xy=TRUE)

#Remove duplicates based on raster cell ID
background_bioclim <- distinct(background_bioclim, cell, .keep_all = TRUE)

#Preview the extracted data for background points
head(background_bioclim)
dim(background_bioclim)

#Add a column indicating the data type (species occurrence = 1, background = 0)
grassClim$presence <- 1
background_bioclim$presence <- 0

#Combine the two datasets
SDMdata <- rbind(grassClim, background_bioclim)

#Preview the combined dataset
head(SDMdata)
dim(SDMdata)
class(SDMdata)

#Check how many presence and background points are in the dataset
table(SDMdata$presence)

#####Number 6#####
library(usdm)

#Select only the bioclimatic variables for the VIFstep process
#Remove the 'presence', 'x', 'y', and 'cell' columns from SDMdata
bioclim_vars <- SDMdata[, c("bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio10", "bio11", "bio15", "bio18", "bio19")]

# Check the structure of the selected bioclimatic variables
str(bioclim_vars)

#Variable selection using VIF
usdm::vifstep(bioclim_vars)
#Variable selection using correlation and VIF
usdm::vifcor(bioclim_vars, th=0.8) 
#Remove problematic variables
remVars <- vifstep(bioclim_vars)@excluded
SDMbioclim <- bioclim_vars[,-which(names(bioclim_vars) %in% remVars)]

#Remove problematic variables from bioRast.aus but keep as a stack
keep <- as.character(vifstep(bioclim_vars)@results$Variables)
bioRasts.aus <- bioRasts.aus[[keep]]

#####Number 7#####
#Load the necessary library
library(ENMeval)

#Extract coordinates of presence points (x, y)
xyPres <- grassClim[, c("x", "y")]

#Extract coordinates of background points (x, y)
xyBg <- background_bioclim[, c("x", "y")]

#Set a seed for reproducibility
set.seed(123)

#k-fold cross-validation partitioning (5 folds)
bv.kfold <- ENMeval::get.randomkfold(occs = xyPres,
                                     bg = xyBg,
                                     k = 5)

#View the structure of the k-fold results
str(bv.kfold)
table(bv.kfold$occs.grp)

#Plot the data
evalplot.grps(pts=xyPres, pts.grp=bv.kfold$occs.grp, envs=stack(bioRasts.aus))

#Make life a little easier
selTrainTest <- as.numeric(unlist(bv.kfold))

#Create a training dataset
sdmData.train <- subset(grassClim, selTrainTest!= 1)
dim(sdmData.train)# Fit Mahalanobis model
class(sdmData.train)

#Create a testing dataset
sdmData.test <- subset(grassClim, selTrainTest!= 1)
dim(sdmData.test)
class(sdmData.train)

######Number 8######
#Create the base map with the raster layer
#Plot the raster layer first (bio2)
plot(bioRasts.aus$bio2, main = "Training and Testing Points on Bioclimatic Raster")
#Plot the background points
points(background_bioclim$x, background_bioclim$y, col = "red", pch = 1, cex = 0.1)
#Plot the training points
points(sdmData.train$x, sdmData.train$y, col = "blue", pch = 19, cex = 1)
#Plot the testing points (if needed)
points(sdmData.test$x, sdmData.test$y, col = "green", pch = 15, cex = 0.5)
legend(x = 115, y = -10,  # Move it outside the plot
       legend = c("Training", "Testing", "Background"), 
       col = c("blue", "red", rgb(0, 1, 0, alpha = 0.3)),  # Make sure the col vector is complete
       pch = c(21, 22, 24),  # Include the pch for the Background
       pt.cex = 1, 
       bty = "o", 
       cex = 1.2)

######Number 9######
?mahal

#Terra to raster
biorast.mm <- stack(bioRasts.aus)
class(biorast.mm)

#Estimate mahalanobis distances on presence only training data (ignore bg)
mahal_model <- mahal(biorast.mm, 
                     cbind(sdmData.train$x, sdmData.train$y))  # Combine x and y into a matrix

#Then do prediction (takes appreciably longer)
mahal_predict <- dismo::predict(stack(biorast.mm), # raster stack
                                 mahal_model, # model 
                                 progress='text')
plot(mahal_predict)
######Number 10######
#Convert the Mahal distance values to p-values
mahal_prob <- app(rast(mahal_predict), function(x, k=nlayers(biorast.mm)){
  x <- 1-x
  x <- x^2
  p_value <- pchisq(x, df = k, lower.tail = FALSE)
  return(p_value)
})

#Plot the p-values
plot(mahal_prob)

######Number 11######
?maxnet

getwd()
filePath <- "/Users/hannahbrunelle/Desktop/SpatialEcology"
#Let's now measure variable importance using *jackknife* and also produce 
#response curves. Running this model will take a minute or two.
mx <- maxent(stack(biorast.mm), 
             xyPres, 
             path=filePath,
             args=c("jackknife", "responsecurves"))

#Plotting the model
plot(mx)

###Question 2: According to your maxent model, which are the two most important variables associated with the species distribution of the Austral grass tree?
#Which variable is the least important? 
##Answer: The two most important variables associated with the species distribution of the Austral grass tree are bio 15 and bio 18. 
##The least important variable is bio 6.

###Question 3: Based on your interpretations of the response curves, what can you say about bioclimatic curves, what can you say about 
#bioclimatic controls on the distribution of the Austral grass tree?
##Answer: The response curves show how the probability of presence of the species changes with respect to the bioclimatic variables.
##The results from the response curves indicate that bio 15 and bio 18 are the most important predictors of the species distribution. 
##However, bio 6  has the least amount of influence on the species distribution and presence.

###Question 4: Compare the predict distributions from the two SDMs.
##Answer: The prediction distributions from the two SDMs show that the species distribution is influenced by different bioclimatic variables.
##The Mahalanobis model measures the distances between the presence points and the distribution, which allows for a more nuanced interpretation. In this case,
##the Mahalanobis model shows that there is a high probability of grass presence in the areas with 1.0 (yellow on the plot). Whereas, the maxtent model shows that the
##species distribution is influenced by bio 15 and bio 18 and this is where you are most likely going to find the grass species. Some ways you may address modeling errors would be
#to (1)ensure collinearity isn't an issue in the Mahalanobis model, and (2)make sure the correct decisions were made and that the data was cleaned properly for the Maxtent.

######Number 12######
#Estimate Mahalanobis distances on testing data
mahal_model2 <- mahal(biorast.mm, 
                     cbind(sdmData.test$x, sdmData.test$y))  # Combine x and y into a matrix

#Predictions for testing data
mahal_predict2 <- dismo::predict(stack(biorast.mm), # raster stack
                                mahal_model2, # model 
                                progress='text')
plot(mahal_predict2)

#Convert the Mahalanobis distance values to p-values
mahal_prob2 <- app(rast(mahal_predict2), function(x, k=nlayers(biorast.mm)){
  x <- 1-x
  x <- x^2
  p_value <- pchisq(x, df = k, lower.tail = FALSE)
  return(p_value)
})

#Plot the p-values
plot(mahal_prob2)

#Maxent on testing data
mx2 <- maxent(stack(biorast.mm), 
              xyBg, 
             path=filePath,
             args=c("jackknife", "responsecurves"))

#Plotting the model
plot(mx2)

###Question 5: Discuss the model evaluation metrics. Which model performed best? 
##Answer: I would say that the Mahalanobis model performed best compared to the maxtent model because the 
#AUC of the maxtent model was 0.5 indicating that the climate variables were no different than random. 
#This discrepancy in the visualization of the Mahalanobis model and the maxtent model may be because the Mahalanobis model
#is addressing the similarity across the species range, where the maxtent model may be indicating non-linear effects from the climate variables. 
#In order to address this, I would examine the climate variable correlations more thoroughly because I know the Mahalanobis model struggles with high correlations. 

###Question 6: How might you improve SDMs for the Austral grass tree?
##Answer: To improve the SDMs for the Austral grass tree, I would suggest using another modeling technique to examine the relationship between the species and the bioclimatic variables.
##The classification and regression trees may be useful because they are good at handling interactions between predictors (categorical ones too) and missing data. It would also be beneficial to examine 
##other variables that may effect the species distribution. 
