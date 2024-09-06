#Class Sept. 3, 2024 
#Spatial Ecology in R
#Hannah Brunelle

######Exercise #1 - Create a Raster#####
# all things raster, see also the raster package
library(terra)

# create a raster object by defining nrows and ncols,
# as well as geographic extent
myRast <- terra::rast(ncol=10, nrow=10, xmax=-80, xmin=-150, ymin=20, ymax=60)

#print the raster
print(myRast)
#class of the raster
class(myRast)
#plot the raster
plot(myRast)

######Exercise #2 - Adding Data#####
terra::hasValues(myRast) #confirms if there are values

# add random values to r
values(myRast) <- runif(n=ncell(myRast))
# confirm that the raster now has values
terra::hasValues(myRast)

# plotting the raster 
plot(myRast)

# add sequence of numbers
values(myRast) <- 1:ncell(myRast)
# print myRast
myRast #plot myRast
plot(myRast) 

# value in row 3, col 10
myRast[3,10]
# value in cellID = 38
myRast[c(10, 38, 89)]
# change value in cellID 50 to 100
myRast[50] <- 100

######Exercise #3 - Raster Stacks#####
# You will often use multiple rasters when examining variables (temp, elevation, land cover)
# make copies of myRast
myRast3 <- myRast2 <- myRast
# add random values to each raster
values(myRast2) <- runif(ncell(myRast))
values(myRast3) <- runif(ncell(myRast))
# create a stack
rrr <- c(myRast,myRast2,myRast3)
# print new stack
rrr# how many layers in rrr?
terra::nlyr(rrr)# the class of rrr is the same as that of myRast
class(rrr)

######Exercise #4 - Indexing and Subsetting Raster Stacks#####

######Exercise #5 - Reading and Writing Rasters to disk #####
# path to a raster file that comes with the terra package
# the system.file function is used to locate R system files only
rfile <- system.file("ex/elev.tif", package="terra")
# read the raster file
elev <- terra::rast(rfile)
# plot the raster
plot(elev)
# create a new raster object by adding 100 to the values in elev
elev100 <- elev*100

# create a new raster object by multiplying the values in elev by 2
elev2 <- elev * elev
# plot the new rasters
par(mfrow=c(1,3)) # divide plot window into 1 row, 3 cols
plot(elev, main="elev")
plot(elev100, main="elev * 100")
plot(elev2, main=expression(elev^2))# make a stack of the new rasters
elevStack <- c(elev, elev100, elev2)
# print plot
elevStack

# save elev to disk
terra::writeRaster(elev, "data/elevStack.tif", overwrite=TRUE)
# save elevStack to disk
# this will produce a single file with 3 layers
terra::writeRaster(elevStack, "data/elevStack.tif", overwrite=TRUE)
