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
install.packages("ncf")
#Read in needed packages
library(ncf)

#Read up on the package
??ncf::correlog

# Use terra::crds to get the coordinates from valid_points
coords <- terra::crds(valid_points)
coords

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

#Using terra::distance() to calcualte the distances
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
#When playing around with the increments, when I making the increments larger, the 
#correlogram becomes less detailed and more linear. When I make the increments smaller,
#the correlogram becomes more detailed and shows variation in the line. 
#The correlogram is showing that generally there is a positive correlation between the abundance of 
#Carolina Wrens and the distance between the points. This means that Carolina Wrens generally cluster
#in groups, rather than being random/evenly distributed. When the samples are randomized, the correlogram has a
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
#The variogram is showing that the abundance pattern of the Carolina Wrens is isotropic. 
#Isotropic means that the abundance of the birds are the same in all directions.
#To avoid autocorrelation in the next Carolina Wren study, the sample sites should be the at least the maximum distance apart.
#This should allow for more randomized samples and less autocorrelation.