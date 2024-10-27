# Set the working directory to the master folder
# Uncomment and modify the path below as needed for each student's system
# setwd("path/to/your/speciesdistributionmodels")

# Required libraries
# Uncomment to install packages if needed
# install.packages("sdm")
# install.packages("sp")
# install.packages("raster")
# install.packages("usdm")
# install.packages("CoordinateCleaner")
# install.packages("spThin")
# install.packages("geodata")
# install.packages("sf")
# install.packages("terra")
# install.packages("tidyverse")
# install.packages("rgbif")

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

#------------------------
# Code section 1 - Set up file paths and load bioclimatic data

# Set up paths within the master directory
bioclim_folder <- file.path(getwd(), "climate", "wc2.1_2.5m")
cmip6_file <- file.path(getwd(), "climate", "cmip6_biof6180.tif")
occurdata_file <- file.path(getwd(), "occurdata.csv")

# Create a SpatRaster object from the bioclimatic variables in the bioclim_folder
bio <- rast(list.files(bioclim_folder, pattern = "\\.tif$", full.names = TRUE))
names(bio) <- paste0("bio", 1:19)  # Rename layers
crs(bio) <- "EPSG:4326"  # Set projection to WGS84

# Verify and plot one layer
print(bio)
plot(bio[[1]])

# Load CMIP6 climate projection file
biof6180 <- rast(cmip6_file)
crs(biof6180) <- "EPSG:4326"
print(biof6180)
plot(biof6180)

#------------------------
# Code section 2 - Load occurrence data and prepare spatial points

# Load occurrence data (assumed CSV format in the master folder)
occurdata <- read.csv(occurdata_file)
occurdata_sf <- st_as_sf(occurdata, coords = c("Longitude", "Latitude"), crs = 4326)

# Verify CRS and plot points
print(st_crs(occurdata_sf))
plot(st_geometry(occurdata_sf))

#------------------------
# Code section 3 - Reduce colinearity

# Set the geographic extent based on occurrence data
geographic.extent <- st_bbox(occurdata_sf)
bioc <- crop(bio, y = geographic.extent)
ex <- terra::extract(bioc, occurdata_sf)

# Apply VIF to reduce collinearity
v1 <- vifstep(ex)
bioc_stack <- brick(bioc)
biom <- exclude(bioc_stack, v1)

#------------------------
# Code section 4 - Build and run the SDM

# Convert occurrence data to spatial points
occurdata_sp <- as_Spatial(occurdata_sf)
d1 <- sdm::sdmData(species ~ ., occurdata_sp, predictors = biom, bg = list(method = 'gRandom', n = 10000, remove = TRUE))

# Build model using specified methods
m1 <- suppressWarnings(sdm(species ~ ., d1, methods = c('gam', 'rf'), replication = 'cv', cv.folds = 5, n = 1))
print(m1)

#------------------------
# Code section 5 - Evaluate model and visualize results

eval1 <- sdm::getEvaluation(m1)
roc(m1)

#------------------------
# Code section 6 - Create ensemble models

species_name <- "Pinus_ponderosa"
en1 <- sdm::ensemble(m1, biom, filename = paste0(species_name, "_T1.tif"), setting = list(method = 'weighted', stat = 'tss', opt = 2), overwrite = TRUE)

#------------------------
# Code section 7 - Evaluate predictor importance

getVarImp(m1)
plot(getVarImp(m1, method = 'rf'))

#------------------------
# Code section 8 - Generate Presence/Absence predictions

df <- as.data.frame(d1)
df <- data.frame(presabs = df$species, coordinates(d1))
ev <- sdm::evaluates(df$presabs, terra::extract(en1, df[, c('coords.x1', 'coords.x2')]))

th <- ev@threshold_based$threshold[2]
pa1 <- terra::rast(en1)
pa1[] <- ifelse(en1[] >= th, 1, 0)
terra::writeRaster(pa1, paste0(species_name, "_current_presence.tif"), overwrite = TRUE)

#------------------------
# Code section 9 - Download and prepare U.S. state boundaries for visualization

geojson_url <- "https://www2.census.gov/geo/tiger/GENZ2021/shp/cb_2021_us_state_500k.zip"
download.file(geojson_url, destfile = "cb_2021_us_state_500k.zip")
unzip("cb_2021_us_state_500k.zip")
us_states <- st_read("cb_2021_us_state_500k.shp")

# Set to WGS84 if needed and extract Arizona boundary
us_states <- st_transform(us_states, 4326)
az <- us_states[us_states$STUSPS == "AZ", ]

# Plot current and predicted future occurrence
xlim <- c(-117, -105)
ylim <- c(30, 40)
par(mfrow = c(1, 2), mar = c(1, 4, 1, 3))

en1_crop <- mask(en1, az)
plot(en1_crop, xlim = xlim, ylim = ylim, main = "Ponderosa habitat suitability")
plot(pa1, xlim = xlim, ylim = ylim, main = "Ponderosa predicted occurrence")

#------------------------
# Future predictions and additional visualization setup

# Crop and rerun model for future climate
biof6180c <- crop(biof6180, y = geographic.extent)
names(biof6180c) <- names(bio)
enbiof6180 <- sdm::ensemble(m1, biof6180c, filename = "pondo_future.tif", setting = list(method = 'weighted', stat = 'tss', opt = 2), overwrite = TRUE)

# Final plots for current and future distributions
par(mfrow = c(1, 2), mar = c(0, 0, 1, 0))
plot(mask(en1, az), col = colorRampPalette(c('#3E49BB', '#3498DB', 'yellow', 'orange', 'red', 'darkred'))(200), main = "Current Ponderosa habitat")
plot(mask(enbiof6180, az), col = colorRampPalette(c('#3E49BB', '#3498DB', 'yellow', 'orange', 'red', 'darkred'))(200), main = "Future Ponderosa habitat")