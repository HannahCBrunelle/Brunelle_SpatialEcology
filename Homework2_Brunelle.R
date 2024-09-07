
# extract the values for the karri records
grass.bioclim <- terra::extract(ausBioclimMasked, grass.aus, cells=TRUE)
# check the data
head(grass.bioclim)# summarize the extracted data. Any NAs?
summary(grass.bioclim)

# add the raster values to the karri.sw object
grass.aus <- cbind(grass.aus, grass.bioclim)
plot(grass.bioclim)




# assign CRS information
sf::st_crs(austral_grass_tree_records.sf) <- 4326 #WGS84
st_crs(austral_grass_tree_records.sf) <- 4326

#Troubleshooting because the plot was not working but the crs was not assigned
class(austral_grass_tree_records.sf)
st_geometry(austral_grass_tree_records.sf)

# Save the sf object as a shapefile # append overwrite the old shapefile
st_write(occurrence_sf, "xanthorrhoea_australis_cleaned.shp", append = FALSE)

