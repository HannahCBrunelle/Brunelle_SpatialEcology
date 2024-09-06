#Class Sept. 3, 2024 
#Spatial Ecology in R
#Hannah Brunelle

######Exercise #1 - Creating some data#####
# set random number seed
set.seed(809)

# longitude object
longitude <- runif(n=10, min=-120, max=-115)

# latitude object
latitude <- runif(n=10, min=35, max=45)

# combine longitude and latitude into a matrix
lonlat <- cbind(longitude, latitude)

# print the result
print(lonlat)


#####Exercise #2 - Making a lonlat into a spatial object#####
# load sf
library(sf)

# run these outside of this tutorial
# what vignettes are available?
#vignette(package="sf")

# introduction to sf
#vignette("sf1")

# convert lonlat to data frame
lonlat.df <- as.data.frame(lonlat)

# convert lonlat.df to spatial object
lonlat.sf <- sf::st_as_sf(lonlat.df, coords = c("longitude", "latitude"))

# print the result
print(lonlat.sf)

#Print the first few rows of lonlat.sf.
#Print what class the lonlat.sf object is.
#Make a plot of lonlat.sf.
head(lonlat.sf) # or lonlat.sf[1:5,]
class(lonlat.sf)
plot(lonlat.sf)

######Exercise #3 - Coordinacne Reference System (CRS)#####
# get attributes of the CRS
sf::st_crs("EPSG:4326")
# assign the CRS for lonlat.sf
sf::st_crs(lonlat.sf) <- 4326
# print the result
print(lonlat.sf)

######Exercise #4 - Adding Attributes to a Spatial Object #####
# create the data frame
climate <- data.frame(temperature = rnorm(n=10, mean=25, sd=5), precip = runif(10, min=0, max=500))
# join the climate data to lonlat.sf
climatePts <- lonlat.sf %>%
  dplyr::mutate(climate) #Adding a column to a new dataframe 
# print the result
print(climatePts)

# ggplot2 
library(ggplot2)
# plot the result
ggplot(climatePts) +
  geom_point(aes(x = longitude, y = latitude, color = temperature, size = precip)) +
  scale_color_viridis_c() +
  scale_size_continuous(range = c(1, 10)) +
  theme_minimal()

######Exercise #5 - Creating a polygon #####
# Define coordinates for the vertices of a polygon
# Note that the polygon must be closed by repeating the first point at the end
x <- c(0, 2, 5, 8, 9, -5, -3, 0)
y <- c(0, 3, 7, 8, 11, 3, 1, 0)

# Combine the x and y coordinates into a matrix
coords <-cbind(x, y) 

# Create a polygon geometry
poly_geom <- sf::st_polygon(list(coords))

# Convert to an sf object
poly.sf <- sf::st_sf(geometry = st_sfc(poly_geom))

# Print the result
print(poly.sf)#
# Plot the result
plot(poly.sf)

######Exercise #6 - Reading/Writing a Vector Shapefile #####
# read the shapefile using st_read
nc <- sf::st_read(dsn = "data/nc.shp", )# print counties in nc
print(nc)

# Printing the shape file
ggplot(nc) +
  geom_sf(aes(fill = AREA)) +
  theme_minimal()

# extract the AREA column
nc.area <- nc %>% 
  dplyr::select(AREA)

# print the result
print(nc.area)# save the subset as a new shapefile
sf::st_write(nc.area, "data/nc_area.shp", delete_layer=T)
