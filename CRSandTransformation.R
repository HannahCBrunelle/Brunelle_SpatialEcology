#Class Sept. 3, 2024 
#Spatial Ecology in R
#Hannah Brunelle

#####IMPORTANT 
#1#When performing spatial analyses, it is essential to know the CRS of the data and to answer a few basic questions: 
#Do all datasets involved have the same CRS?Is the CRS a geographic or projected system? Which CRS is most appropriate for the analyses and why?
#2#When working with multiple spatial datasets, it is crucial to ensure that they are in the same CRS
#3#When transforming spatial data from one CRS to another, it is essential to understand the implications of the transformation on the data, which will add distortion in terms of area, direction, distance, and shape.
#4#When faced with the choice to transform vector or raster data, it is generally better to transform the vector data to the CRS of the raster data than vice versa.

######Exercise #1 - The differance between geographic and projected CRS#####
# create a simple point in London
london <- data.frame(lon = -0.1, lat = 51.5) %>% 
  sf::st_as_sf(coords = c("lon", "lat"))
# check the CRS of the point
sf::st_crs(london)# check if the CRS is long-lat (geographic) or projected
sf::st_is_longlat(london)

# set the CRS of the London point to WGS84 (EPSG code = 4326)
london_geo <- sf::st_set_crs(london, value=4326)
# check if the CRS is longlat
sf::st_is_longlat(london_geo)# check the CRS of the point
sf::st_crs(london_geo)

# project from WGS84 to British National Grid
london_proj <- sf::st_transform(london_geo, crs = 27700)
# here is the same result, but using the proj4string
london_proj_proj4 <- sf::st_transform(london_geo, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs +type=crs")

# print the object
print(london_proj)#
# print the CRS of the object
sf::st_crs(london_proj)

# buffer the point by 1 degree
london_buff_lonlat <- sf::st_buffer(london_geo, dist = 1)
# buffer the point by 100,000 meters
london_buff_proj <- sf::st_buffer(london_proj, dist = 1e5)

######Exercise #2 - Transforming Raster Data #####
# create a simple raster
r <- terra::rast(ncol=100, nrow=100, xmax=-80, xmin=-150, ymin=20, ymax=60)
# set the cell values to the cell number
values(r) <- 1:ncell(r)
# print the CRS of the raster
terra::crs(r, describe=TRUE, proj=TRUE)#
#print the raster
r#
# plot the raster
plot(r)

# define new projection
newproj <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
# 'project' function from terra package
r.proj <- terra::project(r, newproj)
# print the crs details
terra::crs(r.proj, describe=TRUE, proj=TRUE)#
#print the raster
r.proj#
# plot the projected raster
plot(r.proj)
# print original raster
print(r)# print projected raster
print(r.proj)
# print original raster
print(r)# print projected raster
print(r.proj)

# load nlcd raster
nlcd <- terra::rast("data/nlcd.tif")
# print the nlcd raster
print(nlcd)# plot the raster
plot(nlcd)# project the raster & use nearest neighbor to retain cell values
nlcd.near <- terra::project(nlcd, "EPSG:4326", method="near")
# for demonstration purposes, using bilinear interpolation
nlcd.bilin <- terra::project(nlcd, "EPSG:4326", method="bilinear")
# plot the projected rasters
par(mfrow=c(1,2))
plot(nlcd.near, main = "nearest neighbor")
plot(nlcd.bilin, main = "bilinear")#
# print the nlcd.near raster
print(nlcd.near)#
# print the nlcd.bilin raster
print(nlcd.bilin)

#create a new raster `a`
a <- rast(ncols=40, nrows=40, xmin=-110, xmax=-90, ymin=40, ymax=60, crs="+proj=longlat +datum=WGS84")
# set the cell values to the cell number
values(a) <- 1:ncell(a)
# print a
print(a)# new CRS in proj4 format
newcrs <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84"
# create a new raster `b` with the new CRS
b <- rast(ncols=94, nrows=124, xmin=-944881, xmax=935118, ymin=4664377, ymax=7144377, crs=newcrs)
# print b
print(b)# project the a raster using to the new CRS
a.proj <- project(a, newcrs)
# project the a raster using the template raster b
a.proj.bTemplate <- project(a, b)
# print the projected raster using the new CRS
print(a.proj)# print the projected raster using the template raster b
print(a.proj.bTemplate)# plot the projected rasters
par(mfrow=c(1,2))
plot(a.proj, main = "projected using new CRS")
plot(a.proj.bTemplate, main = "projected using template raster b")
