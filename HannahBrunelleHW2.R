#Spatial Ecology - Homework #2 
#Date: September 10, 2024
#Hannah C Brunelle
#Contact: hannahb7@umbc.edu
#Resources used: ChatGPT, class activity from Week 2 and GitHub Copilot 

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

#check the data
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

#Filter to keep only Australia
ausPoly <- ausPoly[ausPoly$NAME == "Australia", ]
#Plot the filtered shapefile
plot(ausPoly, main="Australia Only")

##############Number 3####################
#Download occurrence records for Xanthorrhoea australis and cleaning the data
austral_grass_tree_records <- geodata::sp_occurrence(genus="Xanthorrhoea",
                                                     species= "australis",
                                                     download=T,
                                                     geo=T, #Only has records with coordinates
                                                     removeZeros = T) #Removes errors in the data

#check the data
class(austral_grass_tree_records)
#check the dimensions
dim(austral_grass_tree_records)#  
#see how many records from different datasets
table((austral_grass_tree_records$datasetName)) #where did these come from

#convert to sf object
austral_grass_tree_records.sf <- sf::st_as_sf(austral_grass_tree_records.sf, coords = c("lon", "lat"))

#Remove duplicates
austral_grass_tree_records.sf <- austral_grass_tree_records.sf[!duplicated(austral_grass_tree_records.sf),]
#Remove records with invalid years
austral_grass_tree_records.sf <- austral_grass_tree_records.sf[!is.na(austral_grass_tree_records.sf$year) & austral_grass_tree_records.sf$year > 1900 & austral_grass_tree_records.sf$year <= as.numeric(format(Sys.Date(), "%Y")), ]

#use dplyr::select to select only the columns we need
austral_grass_tree_records.sf <- austral_grass_tree_records %>%
  select(acceptedScientificName, institutionCode, year, lat, lon)

#Print the data
print(austral_grass_tree_records.sf)

#check which CRS(s) the data is in
unique(austral_grass_tree_records$geodeticDatum) #what is the CRS because we need this before we can make into a SF (spatial) 

#transform to the same CRS as the auspoly object
austral_grass_tree_records.Transformed <- st_transform(austral_grass_tree_records.sf, 9001)

#save the sf file as a shapefile - it says it was 3D so making it into 2D
st_write(austral_grass_tree_records.Transformed, "/Users/hannahbrunelle/Desktop/austral_grass_tree_records.shp")
# Convert to 2D 
austral_grass_tree_records.Transformed_2D <- st_zm(austral_grass_tree_records.Transformed, drop = TRUE)
#Write to shapefile - Have to overwrite this because it saved it before
st_write(austral_grass_tree_records.Transformed_2D, "/Users/hannahbrunelle/Desktop/austral_grass_tree_records.shp", append = FALSE)

##############Number 4####################
st_crs(ausPoly) #9001
st_crs(austral_grass_tree_records.Transformed) #9001
st_crs(bio_climVars_Stack) #9122

#transform the data
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

#####Plot that has the bioclimatic data (bio 10) and the grass tree records by year of Australia#####
#Create a color ramp for the years
colors <- colorRampPalette(c("blue", "red"))(length(unique(austral_grass_tree_records.sf$year)))
#Match colors to the years in the data
year_colors <- colors[as.factor(austral_grass_tree_records.sf$year)]
#Plot the raster
plot(ausBioclim$wc2.1_5m_bio_10)
#Add points with colors based on years
plot(st_geometry(austral_grass_tree_records.sf), col = year_colors, add = TRUE)
#Add a title to the plot
title(main = "Mean temperature and grass tree occurance by year in Australia", line= 3)

#Bio10 to a GEOTIFF
#Define the file path for the output GeoTIFF
output_file <- "/Users/hannahbrunelle/Desktop/SpatialEcology/output_file_bio10.tif"
#Save the raster layer as a GeoTIFF
writeRaster(ausBioclim$wc2.1_5m_bio_10, output_file, overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES"), datatype='INT1U')
#Check if the file was successfully saved
file.exists("/Users/hannahbrunelle/Desktop/SpatialEcology/output_file_bio10.tif")
#Read the GeoTIFF file
saved_raster <- rast("/Users/hannahbrunelle/Desktop/SpatialEcology/output_file_bio10.tif")
#Print the raster object to check its details
print(saved_raster)
#Plot the raster to verify its contents visually
plot(saved_raster)

##############Number 5####################
#Make a plot with bio_climVars_Stack and austral_grass_tree_records.sf
plot(ausBioclim)

#Extract bioclimatic values for species occurrence points
species_bioclim <- extract(ausBioclim, austral_grass_tree_records.sf)
plot(species_bioclim)

#Make a scatterplot of the extracted bioclimatic values
ggplot(data = species_bioclim, aes(x = wc2.1_5m_bio_10, y = wc2.1_5m_bio_11)) +
  geom_point() +
  xlab("Mean Temperature of Warmest Quarter") +
  ylab("Mean Temperature of Coldest Quarter") +
  ggtitle("Bioclimatic Variables for Xanthorrhoea australis Occurrence Points")

#Make another scatterplot of the extracted bioclimatic values
ggplot(data = species_bioclim, aes(x = wc2.1_5m_bio_18, y = wc2.1_5m_bio_19)) +
  geom_point() +
  xlab("Precipitation of Warmest Quarter") +
  ylab("Precipitation of Coldest Quarter") +
  ggtitle("Bioclimatic Variables for Xanthorrhoea australis Occurrence Points")

#Make another scatterplot of the extracted bioclimatic values
ggplot(data = species_bioclim, aes(x = wc2.1_5m_bio_10, y = wc2.1_5m_bio_18)) +
  geom_point() +
  xlab("Mean Temperature of Warmest Quarter") +
  ylab("Precipitation of Warmest Quarter") +
  ggtitle("Bioclimatic Variables for Xanthorrhoea australis Occurrence Points")

#Make another scatterplot of the extracted bioclimatic values
ggplot(data = species_bioclim, aes(x = wc2.1_5m_bio_11, y = wc2.1_5m_bio_19)) +
  geom_point() +
  xlab("Mean Temperature of Coldest Quarter") +
  ylab("Precipitation of Coldest Quarter") +
  ggtitle("Bioclimatic Variables for Xanthorrhoea australis Occurrence Points")

####Answers to Number 5
#The scatterplots show the relationship between different bioclimatic variables for the occurrence points of Xanthorrhoea australis.
#I only use the BioStack data so I examined the temperatures and precipitation of the warmest and coldest quarters.
#First scatterplot inidcates that the grass prefers the middle temperatures in the warmest quarter and the coldest (~12 degrees C to 20 degrees C).
#Second scatterplot shows that the grass prefers the lower precipitation in the warmest quarter and middle precipitation in the coldest (~200mm to 300mm).
#The species prefers a colder environment with moderate precipitation which examining ausBioClim plots makes sense where their occurance is. 
plot(ausBioclim)

##############Number 6####################
installed.packages("rasterize")
library(rasterize)
library(terra)
library(sf)

#####Attempting with terra::rasterize#####
#Convert sf object to SpatVector
austral_grass_tree_vect <- vect(austral_grass_tree_records.sf)
class(austral_grass_tree_vect)

#Define extent manually based on the known extent of Australia
aus_extent <- ext(aus_extent <- ext(112, 155, -44, -9) ) 

#Create a template raster with 5-minute resolution (1/12 degree)
r_template <- rast(extent = aus_extent, 
                   resolution = 1/12,  # 5 arc-minutes in degrees
                   crs = "EPSG:4326")  # Assuming coordinates are in WGS84

#Rasterize the points to count the number of records in each cell
count_raster <- rasterize(austral_grass_tree_vect, r_template, fun = "length")

#Plot the resulting raster with ausPoly in the background
plot(count_raster)

#Convert the count_raster to a data frame for ggplot
count_raster_df <- as.data.frame(count_raster, xy = TRUE, na.rm = TRUE)

# Plot the raster with ausPoly in the background
ggplot() +
  # Plot the raster counts using geom_tile
  geom_tile(data = count_raster_df, aes(x = x, y = y, fill = V1_length)) +
  scale_fill_viridis_c(option = "plasma", name = "Record Counts") +  # Optional: change color palette
  coord_equal() +
  theme_minimal() +
  labs(title = "Xanthorrhoea australis GBIF Records in 5' Grid Cells",
       x = "Longitude", y = "Latitude") +
  
  # Plot the Australia polygon using geom_sf
  geom_sf(data = ausPoly, fill = NA, color = "black", size = 0.2) 

#####Attempting with manually#####
# Convert sf object to SpatVector
austral_grass_tree_vect <- vect(austral_grass_tree_records.sf)

# Create a template raster with the same extent and resolution as the occurrence data
cell_ids <- cellFromXY(r_template, terra::crds(austral_grass_tree_vect))

# Count the number of occurrences for each unique cell
cell_counts <- table(cell_ids)
class(cell_counts)

# Create a raster with counts, using the cell IDs
count_raster <- r_template
values(count_raster) <- 0  # Initialize all cells to zero

# Create a numeric vector of cell counts that matches the raster cells
# Ensure that the vector length matches the number of raster cells
count_values <- numeric(ncell(count_raster))  # Initialize with zeros

# Assign counts to the corresponding cells
count_values[as.numeric(names(cell_counts))] <- as.numeric(cell_counts)
cell_values <- as.numeric(cell_counts)  

# Assign counts to the corresponding cells
count_raster[cell_counts] <- cell_values

#check the raster
head(count_raster)
summary(count_raster)
