#Spatial Ecology - Homework #2 
#Date: September 17, 2024
#Hannah C Brunelle
#Contact: hannahb7@umbc.edu
#Resources used: ChatGPT, GitHub Copilot, https://en.wikipedia.org/wiki/Correlogram,
#Class activity on Sept. 12, 2024 and https://www.supergeotek.com/Spatial_Statistical_ENG_HTML/isotropy_and_anisotropy.htm

#Read in needed packages 
library(geodata)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(dplyr)
library(terra)

##############Number 1####################
# Read the .tif file
tif <- rast("~/Desktop/carolinaWren.tif")

plot(tif, main = "Carolina Wren Abundance")

#Draw a polygon interactively to define the region of interest
drawn_area <- terra::draw()
#Plot the area
plot(drawn_area, add = TRUE, border = "red")

# Mask the raster using the drawn polygon
tif_masked <- mask(tif, drawn_area)
plot(tif_masked, main = "Masked Carolina Wren Abundance") #checking to see it worked

# Sample points from the masked raster
sampled_points <- spatSample(tif_masked, size = 500, method = "regular", as.points = TRUE, na.rm = TRUE)

##### NOTE: The number of sampled points is << than 500
# Because you sampled the raster and not only within the extent of
# the drawn_area (as shown on the HW handout), you ended up with << 500 points.
# This is likely to greatly impact the rest of the analyses, but we'll see 
# what happens. All that said, I don't know what you used for your "drawn_area".

# also note that after masking, the new and original rasters have the same extent
ext(tif)
ext(tif_masked)

# Plot the sampled points
points(sampled_points, col = "red", pch = 16)

#Summary Plot
plot(tif, main = "Carolina Wren Abundance")
points(sampled_points, col = "red", pch = 16)

##############Number 2####################
# Extract abundance values at the sampled points
abundance_values <- extract(tif_masked, sampled_points)
#Check to see it it worked
plot(abundance_values, col = "red", pch = 16)
class(abundance_values)

# Keep only points with abundance greater than 0 and not in water (assumed NA)
valid_points <- sampled_points[!is.na(abundance_values[, 2]) & abundance_values[, 2] > 0, ]
#Check to see it it worked
plot(valid_points, col = "red", pch = 16)
class(valid_points)

# Plot the raster and valid sample points on the map
plot(tif, main = "Carolina Wren Abundance and Valid Sampling Locations")
points(valid_points, col = "grey", pch = 16)  # Plot only valid sampling points

##############Number 3####################
#Install Package
#install.packages("ncf")
#Read in needed packages
library(ncf)

#Read up on the package
??ncf::correlog

# Use terra::crds to get the coordinates from valid_points
coords <- terra::crds(valid_points)
coords

##### NOTE: Again, it should have been apparent here as well that your code
# produced only a few dozen points instead of the ~500 we were aiming for.

#This Correlogram was not working due to a mismatch in row #'s between coords and abundance_values[, 2]
cor <- correlog(x = coords[,1], 
                y = coords[,2], 
                z = abundance_values[, 2],
                increment = 10000, 
                resamp = 1000)

#Check if the lengths of coords and abundance_values[, 2] match
nrow(coords) == length(abundance_values[, 2]) #FALSE
nrow(coords)  #Number of coordinates - #50
length(abundance_values[, 2])  #Number of abundance values #56

#Find the common valid rows
valid_rows <- seq_len(min(nrow(coords), length(abundance_values[, 2])))
length(valid_rows) #50

#Subset both coords and abundance_values to make sure they align
coords_clean <- coords[valid_rows, ]
abundance_clean <- abundance_values[valid_rows, 2]

#Run the correlogram with cleaned version 
cor <- correlog(x = coords_clean[,1], 
                y = coords_clean[,2], 
                z = abundance_clean,
                increment = 500, 
                resamp = 1000)
plot(cor)

#Using terra::distance() to calculate the distances
library(terra)
pairwise_distances <- terra::distance(valid_points)
max_distance <- max(pairwise_distances) #the max distance 
increment <- max_distance / 15 #dividing the max distance by ~10-20

#Running the correlogram with the right increment
cor <- correlog(x = coords_clean[,1], 
                y = coords_clean[,2], 
                z = abundance_clean,
                increment = increment, 
                resamp = 1000)
#Plotting the correlogram
plot(cor)

#Playing around with the increment value to see how it affects the correlogram
cor <- correlog(x = coords_clean[,1], 
                y = coords_clean[,2], 
                z = abundance_clean,
                increment = increment * 2, 
                resamp = 1000)
plot(cor)

#Playing around with the increment value to see how it affects the correlogram
cor <- correlog(x = coords_clean[,1], 
                y = coords_clean[,2], 
                z = abundance_clean,
                increment = increment * 5, 
                resamp = 1000)
plot(cor)

#Playing around with the increment value to see how it affects the correlogram
cor <- correlog(x = coords_clean[,1], 
                y = coords_clean[,2], 
                z = abundance_clean,
                increment = increment / 2, 
                resamp = 1000)
plot(cor)

#Playing around with the increment value to see how it affects the correlogram
cor <- correlog(x = coords_clean[,1], 
                y = coords_clean[,2], 
                z = abundance_clean,
                increment = increment / 4, 
                resamp = 1000)
plot(cor)

#Answers to Questions:
#The correct increment for my correlogram is 184951.9 (based on line 102). 
#When I made the increments larger, the correlogram became less detailed with points and when I made the increments smaller,
#the correlogram became more detailed and showed more variation in the line. 
#The correlogram is showing that generally there is a positive correlation between the abundance of 
#Carolina Wrens and the distance between the points. 

##### NOTE: You are answer here reflects the fact that too few sample points were used. Please
# check the solution.

#When the samples are randomized, the correlogram has a
#flatter line around 0.0 and then remains negative for the distance, with a steep drop off. (I tested this by changing the "regular" code to "random" code in line 32 and then reran the code until line 111)

##############Number 4####################
install.packages("gstat")
library(gstat)
library(sf)

#Create a data frame with abundance and coordinates
data <- data.frame(abundance_clean = abundance_clean, x = coords_clean[,1], y = coords_clean[,2])
class(data)
print(data)

#Convert to sf object
data_sf <- st_as_sf(data, coords = c("x", "y"))
class(data_sf) #checking class
print(data_sf) #viewing the sf
crs(data_sf) #no crs assigned

#Assigning the CRS
st_crs(data_sf) <- 4326
st_crs(data_sf) #making sure it worked

#Calculate the variogram
v <- variogram(abundance_clean ~ 1, data = data_sf)
plot(v)

#Answers to questions
#The variogram is showing that the abundance of the Carolina Wrens is isotropic due to the fact that the variogram doesn't show any directions.
#To avoid autocorrelation in the next Carolina Wren study, the sample sites should be apart at least the maximum distance.
#This should allow for more randomized samples and less autocorrelation.

##### Please see the solution.