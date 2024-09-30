# Build a species distribution model for Ponderosa pines

#install.packages("sdm")
#install.packages("sp")
#install.packages("raster")
#install.packages("usdm")
#install.packages("CoordinateCleaner")
#install.packages("spThin")
#install.packages("geodata")
#install.packages("sf")
#install.packages("terra")
#install.packages("tidyverse")
#install.packages("rgbif")
#install.packages("usdm")
#installAll(sdm)

library(sdm)
library(sp)
library(raster)
library(usdm)
library(CoordinateCleaner)
library(spThin)
library(geodata)
library(sf)
library(terra)
library(tidyverse)
library(rgbif)
library(usdm)

# First download your files, then use the script below to set a path to your folder
# Code section 1

# Set the path to your folder containing the TIFF files
bioclim_folder <- "~/Desktop/sdmdownloads/climate/wc2.1_2.5m"

# Create a SpatRaster object from the bioclimatic variables
bio <- rast(list.files(bioclim_folder, pattern = "\\.tif$", full.names = TRUE))

# Rename layers
names(bio) <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", 
                          "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")

# Set a projection (WGS 84 - EPSG:4326)
crs(bioclim_stack) <- "EPSG:4326"

# Check the object to ensure everything is correct
print(bioclim_stack)

# Plot one of the layers to confirm the data looks good
plot(bioclim_stack[[1]])  # For example, plotting bio1

# Set the path to the single TIFF file
cmip6_file <- "~/Desktop/sdmdownloads/climate/cmip6_biof6180.tif"

# Load the file as a SpatRaster object
biof6180 <- rast(cmip6_file)

# Set a projection (if needed, otherwise skip this step)
crs(biof6180) <- "EPSG:4326"  # WGS 84

# Check the object to ensure everything is correct
print(biof6180)  # Corrected to use 'biof6180', not 'cmip6_biof6180'

# Plot to visualize the data
plot(biof6180)
#------------------------

# Let's download the occurrence data now

# Step 1: Import the occurrence data (assuming it’s in a CSV format)
# Replace 'path_to_occurdata.csv' with the actual path to your occurdata file
occurdata <- read.csv("/Users/sks379/Desktop/sdmdownloads/occurdata.csv")

# Step 2: Convert the data to a spatial points dataframe using the correct column names for longitude and latitude
occurdata_sf <- st_as_sf(occurdata, coords = c("Longitude", "Latitude"))

# Step 3: Set the CRS to WGS 84 (EPSG:4326)
st_crs(occurdata_sf) <- 4326

# Step 4: Check the coordinate reference system to ensure it is set correctly
print(st_crs(occurdata_sf))

# Optional: Print the first few rows of the spatial points dataframe to confirm
head(occurdata_sf)

# Step 5: (Optional) Plot the occurrence data to visually verify the spatial points
plot(st_geometry(occurdata_sf))

#change to a spatial point dataframe
occurdata <- st_as_sf(occurdata, coords = c("Longitude", "Latitude"))

#crs_wgs84 <- sf::st_crs(4326)  # WGS 84 has EPSG code 4326 
st_crs(occurdata) <- 4326
st_crs(occurdata) #check coordinate string

#--------------------------------
# Reduce colinearity

#reduce colinearity
geographic.extent <- extent(occurdata) #could also use st_bbox
bioc <- crop(x = bio, y = geographic.extent)
ex <- terra::extract(bioc, occurdata)
v1 <- vifstep(ex) #vifstep and vifcor are both methods to remove colinearity - default vif threshold for vifcor is 0.9, but you can change
bioc_stack <- brick(bioc)
biom <- exclude(bioc_stack, v1)

# Build the sdm
occurdata_df <- sf::as_Spatial(occurdata)
d1 <- sdm::sdmData(species~., occurdata_df, predictors = biom, bg=list(method='gRandom', n=10000, remove=TRUE))

#-------------------------------
# Run the model
sdm::getmethodNames()

#create model
m1 <- suppressWarnings(sdm(species~., d1, methods=c('gam','rf'), replication='cv', cv.folds=5, n=1))

#call the model to check it out
m1

#----------------------------------
# Evaluate the model

eval1 <- sdm::getEvaluation(m1)
roc(m1)

#------------------------------------
# Create ensemble models

en1 <- sdm::ensemble(m1, biom, filename = paste(species_name,"_T1.tif", sep=""), setting=list(method='weighted', stat='tss', opt=2), overwrite = TRUE)
#weighting models by tss threshold defined using option 2

#------------------------------------
#look at predictor variables 
getVarImp(m1)
plot(getVarImp(m1, method='rf'))

#-----------------------------------
# Change to predictions of Presence / Absence 

df <- as.data.frame(d1)
df <- data.frame(presabs=df$species, coordinates(d1))
ev <- sdm::evaluates(df$presabs, terra::extract(en1, df[,c('coords.x1', 'coords.x2')]))

th <- ev@threshold_based$threshold[2] 
pa1 <- terra::rast(en1)
pa1[] <- ifelse(en1[] >= th, 1, 0)

terra::writeRaster(pa1, "Pinus_ponderosa.tif", overwrite=TRUE)

#------------------------------------
# URL for the GeoJSON file containing U.S. state boundaries from Natural Earth
geojson_url <- "https://www2.census.gov/geo/tiger/GENZ2021/shp/cb_2021_us_state_500k.zip"

# Download the GeoJSON file locally
download.file(geojson_url, destfile = "cb_2021_us_state_500k.zip")

# Unzip the downloaded file
unzip("cb_2021_us_state_500k.zip")

# Read the GeoJSON file using sf::st_read
us_states <- st_read("cb_2021_us_state_500k.shp")

#check projection
st_crs(us_states)

#looks good but if you'd need to change, you can use this code
us_states <- st_transform(us_states, 4326)

#check projections to make sure that everything looks good
st_crs(en1)
st_crs(us_states)

#extract Arizona state boundary
az <- us_states[us_states$STUSPS == "AZ", 1]

#you can crop to features to make neater
#crs(en1) <- "epsg:4326"

#arrange view for 2 plots
par(mfrow=c(1,2), mar = c(1, 4, 1, 3)) # bottom, left, top, right

xlim <- c(-117, -105)
ylim <- c(30, 40)
en1 <- crop(x = en1, y = geographic.extent)
en1_crop <- mask(en1, az)
plot(en1_crop, xlim = xlim, ylim = ylim, main = "Ponderosa habitat suitability") #plot of continuous data

xlim2 <- c(-117, -105)
ylim2 <- c(30, 40)
pa1 <- crop(x = pa1, y = geographic.extent)
pa1_crop <- mask(pa1, az)
plot(pa1_crop, xlim = xlim2, ylim = ylim2, main = "Ponderosa predicted occurrence") #plot of presence absence data

#-----------------------------------------
# Crop projections
#let's crop!
biof6180c <- crop(x = biof6180, y = geographic.extent)
names(biof6180c) <- names(bio) # change names
biof6180cf <- brick(biof6180c)

#rerun models for our future climate
enbiof6180 <- sdm::ensemble(m1, biof6180cf, filename = "pondo_future.tif", setting=list(method='weighted', stat='tss', opt=2), overwrite = TRUE)

#------------------------------------------
#you can specify your own colors
cl <- colorRampPalette(c('#3E49BB', '#3498DB', 'yellow', 'orange', 'red', 'darkred'))
#bottom, left, top, right
par(mfrow=c(1,2), bty="l", mar=c(0,0,1,0)) #bottom, left, top, right

xlim <- c(-117, -105)
ylim <- c(30, 40)

en1_crop <- mask(en1, az)
plot(en1_crop, col=cl(200), xlim = xlim, ylim = ylim, main = "Current Ponderosa habitat", box=FALSE, axes=FALSE, legend=FALSE) #plot of continuous data

enbiof6180_crop <- mask(enbiof6180, az)
plot(enbiof6180_crop, col=cl(200), xlim = xlim, ylim = ylim, main = "Future Ponderosa habitat", box=FALSE, axes=FALSE, legend=FALSE) #plot of presence absence data


#trying again using new method
df <- as.data.frame(d1)
df <- data.frame(presabs=df$species, coordinates(d1))
ev2 <- sdm::evaluates(df$presabs, terra::extract(enbiof6180, df[,c('coords.x1', 'coords.x2')]))

th2 <- ev2@threshold_based$threshold[2] 
pa2 <- terra::rast(enbiof6180)
pa2[] <- ifelse(enbiof6180[] >= th2, 1, 0)

terra::writeRaster(pa2, "Pinus_ponderosa_future.tif", overwrite=TRUE)

pa2_crop <- mask(pa2, az)

par(mfrow=c(1,2), bty="l", mar=c(0,0,0,0)) #bottom, left, top, right

xlim <- c(-117, -105)
ylim <- c(30, 40)

# Latitude and longitude coordinates for Flagstaff, AZ
flagstaff_lat <- 35.1983
flagstaff_lon <- -111.6513

plot(pa1_crop, xlim = xlim, ylim = ylim, main = "Current Ponderosa occurrence", box=FALSE, axes=FALSE, legend=FALSE) # Add a point for Flagstaff, AZ
points(flagstaff_lon, flagstaff_lat, pch = 8, col = "black", cex = 2)

plot(pa2_crop, xlim = xlim, ylim = ylim, main = "Future Ponderosa occurrence", box=FALSE, axes=FALSE, legend=FALSE) # Add a point for Flagstaff, AZ
points(flagstaff_lon, flagstaff_lat, pch = 8, col = "black", cex = 2)

