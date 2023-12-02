setwd("Z:/Downloads/Applied Period/Analysis")
#========================================================================
# TASK: ESTIMATION OF CANOPY COVER FROM SATELLITE IMAGES
#========================================================================

rm(list = ls()) #Clean R's memory

# When running the code first time, these additional libraries need to be installed.
# If you didn't install them yet, decomment the lines below, run to install, and 
# comment again. 

# Note that this lab uses an older geospatial library (sp) than the lidR lab (sf). 
# Some of the geoprocessing operation are therefore different.

# install.packages("Rcpp") 
# install.packages("raster")
# install.packages("sp")
# install.packages("rgeos")
# install.packages("rgdal")
# install.packages("randomForest")
# install.packages("yaImpute")
# install.packages("gower")
# install.packages("betareg")
# install.packages("caret")
# install.packages("sf")
# install.packages("terra")

# Load the installed libraries
require(Rcpp) 
require(raster)
require(sp)
require(rgeos)
require(rgdal)
require(randomForest)
require(yaImpute)
require(gower)
require(betareg)
require(caret)
source("lidr_functions.r")
require(sf)
require(terra)

# Define function for computing RMSE and RMSE-%
rmse <- function(x1,x2) { sqrt(sum((x1-x2)^2) / length(x1)) }
relrmse <- function(x1,x2) { sqrt(sum((x1-x2)^2) / length(x1)) / mean(x1) }

#=============================================================
# Extract Sentinel-2 for Joensuu plots 
#=============================================================

# Read in field plots
juu_plots <- read.csv("Joensuu2023.csv")
tail(juu_plots)

# Set coordinate system
coordinates(juu_plots) <- c("x_coord","y_coord")
proj4string(juu_plots) <- CRS("+proj=utm +zone=35 +ellps=GRS80 +units=m +no_defs") # TM35FIN parameters

# Read Sentinel-2
sentinel2 <- brick("Merged_Joensuu_2023_Clipped.tif")

# Plot satellite image and add the field plot locations
plotRGB(sentinel2, r=3, g=2, b=1, stretch="lin") 
plot(juu_plots, col="yellow", pch=16, cex=.75, add=T)

# Set plot radius based on sample plot type
juu_plots$rad <- 12.5

# Convert plot centers to polygons based on the set radius
buffered <- gBuffer(juu_plots, width = juu_plots$rad, byid=T)

# Extract as data frame, get a normalized weight for each pixel within a polygon
image_df <- extract(sentinel2, buffered, df=T, weights=T, normalizeWeights=T) 

# Rename columns
names(image_df) <- c("ID", "blue", "green", "red", "re1", "re2", "re3", "nir", "nirnar", "swir1", "swir2", "weight")
tail(image_df)

# Function for computing the weighted mean by band
wmean <- function(band){ 
  wsum <- image_df[,band] * image_df$weight
  tapply(wsum, image_df$ID, sum)
}
  
# Apply function with the named columns
juu_s2 <- as.data.frame(cbind(juu_plots$plot, wmean("blue"), wmean("green"), wmean("red"), wmean("re1"), wmean("re2"),
                                wmean("re3"), wmean("nir"), wmean("nirnar"), wmean("swir1"), wmean("swir2")))

# Rename columns
names(juu_s2) <- c("plot", "blue", "green", "red", "re1", "re2", "re3", "nir", "nirnar", "swir1", "swir2")

# View last rows
tail(juu_s2)

#=============================================================
# Merge data sets
#=============================================================

# Place plots into new data frame d

d <- cbind(as.data.frame(juu_plots), juu_s2)

#=============================================================
# Construct betareg forest models
#=============================================================

dev.off() # Close previous plots

# Sentinel-2 model

#exh_var_search(d, 3, 'cov', 46, 54) #Excluding blue band

#s2 <- betareg(cov~blue+re1+nirnar+swir2,data=d, link="logit")
#s2 <- betareg(cov~swir2+swir1+nirnar,data=d, link="logit")
s2 <- betareg(cov~nir+swir1+nirnar,data=d, link="logit") #FINAL
#s2 <- betareg(cov~blue+re1+swir1+swir2,data=d, link="logit")
s2.pred <- predict(s2)
plot(d$cov, s2.pred, xlim=c(0,1), ylim=c(0,1)); abline(0,1)
plot(fitted(s2), residuals(s2))
abline(h=0)
summary(s2)

#Compute RMSE
rmse(d$cov, s2.pred)

#Compute RELRMSE
relrmse(d$cov, s2.pred)

#=============================================================
# Reclassify Rasters
#=============================================================

# Read PALSAR-2 bands
hh <- raster("maaluokka_vmi1x_1721_N5.tif")
hv <- raster("maaluokka_vmi1x_1721_P5.tif")

#Merge the rasters
merged_raster <- merge(hh,hv)

#Define the Reclassification Rules
reclass_rules <- cbind(
  from = c(1, 2, 3, NA, 32766),   # Values to be reclassified
  to = c(1, 1, 1, 0, 0)   # New values to assign
)

# Reclassify the raster
reclassed_raster <- reclassify(merged_raster, reclass_rules)

# Visualize the reclassified raster
#plot(reclassed_raster)

#Save the Reclassified Raster
#writeRaster(reclassed_raster, "mask_raster.tif", format = "GTiff")

#=============================================================
# Processing the map data
#=============================================================

plotRGB(sentinel2, r=3, g=2, b=1, stretch="lin")

# Convert raster stack into data frame
imagedataframe <- as.data.frame(sentinel2)

# Add column names for bands
names(imagedataframe) <- c("blue", "green", "red", "re1", "re2", "re3", "nir", "nirnar", "swir1", "swir2")

#Read the masked layer
mask <- raster("mask_raster_clipped.tif")

# Set up a forest mask: areas that are not nodata are forest
mask[mask == 0] <- NA

# Plot the mask
plot(mask)

# Convert also forest mask into data frame
maskdataframe <- as.data.frame(mask)


#=============================================================
# Making the final map
#=============================================================

# Apply the previous model 
pred <- predict(s2, imagedataframe)

# Replace the predicted values for non-forest areas with nodata (Pixel with no data in mask layer will not have predictions, for example in water and roads)
#pred[! maskdataframe$mask_raster_clipped] <- NA

# Construct a new map
# Make a copy of the first image band
ccmap <- sentinel2[[1]] 

# Replace original values by model predictions
values(ccmap) <- pred 

# Show the  map
plot(ccmap) 

# Write the map as a tif file
#writeRaster(ccmap,"Canopy_Cover_Map3.tif",overwrite=T)

