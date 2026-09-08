# =============================================================
#  ENV 226 Lab — Species Distribution Modeling
#  Predicting current and future habitat for Pinus ponderosa
#  near Flagstaff, AZ under SSP 5-8.5
#
#  This script auto-downloads everything it needs.
#  No manual file downloads are required.
# =============================================================

# -------------------------------------------------------------
# 0a. Set your working directory
# -------------------------------------------------------------
#
# This script needs to find "occurdata.csv", which lives in the
# same folder as this script.
#
# EASIEST WAY (recommended):
#   In RStudio, click:  Session  >  Set Working Directory
#                       >  To Source File Location
#   That's it. No code needed.
#
# OR — uncomment the line below and paste the path to the folder
#      containing this script (e.g., "~/Desktop/SDM lab exercise"):
#
# setwd("~/Desktop/SDM lab exercise")

# -------------------------------------------------------------
# 0b. Setup — install and load packages
# -------------------------------------------------------------

# Run this block ONCE per computer. It installs everything you need.
# (Comment out after the first successful run.)
required_pkgs <- c(
  "sf", "terra", "raster", "sp",
  "sdm", "usdm", "dplyr",
  "geodata", "rnaturalearth", "rnaturalearthdata",
  "dismo", "gbm", "randomForest"
)
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs) > 0) install.packages(new_pkgs)

# IMPORTANT: the sdm package needs to install its underlying ML engines.
# Run this once after installing sdm. It pulls down brt, rf, glm, maxent, etc.
if (!file.exists(file.path(find.package("sdm"), "methods", "sdm", "brt.R"))) {
  sdm::installAll()
}

# Load packages
suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(raster)
  library(sp)
  library(sdm)
  library(usdm)
  library(dplyr)
  library(geodata)
  library(rnaturalearth)
})

# Reproducibility — fixes the random background points and CV folds
set.seed(2026)

# Cache folder for downloaded climate + boundary data.
# This keeps everything in one place and prevents re-downloading.
cache_dir <- file.path(tempdir(), "sdm_cache")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

# -------------------------------------------------------------
# 1. Download current bioclimatic variables (WorldClim v2.1)
# -------------------------------------------------------------
bio <- geodata::worldclim_global(var = "bio", res = 2.5, path = cache_dir)
names(bio) <- paste0("bio", 1:19)
crs(bio) <- "EPSG:4326"

# Quick visual check
plot(bio[[1]], main = "BIO1 — Annual Mean Temperature (°C * 10)")

# -------------------------------------------------------------
# 2. Download future climate projection (CMIP6, SSP 5-8.5, 2041-2060)
# -------------------------------------------------------------
biof6180 <- geodata::cmip6_world(
  var   = "bio",
  res   = 2.5,
  ssp   = "585",
  model = "BCC-CSM2-MR",
  time  = "2041-2060",
  path  = cache_dir
)
names(biof6180) <- paste0("bio", 1:19)
crs(biof6180) <- "EPSG:4326"

# -------------------------------------------------------------
# 3. Download U.S. state boundaries (no manual download needed)
# -------------------------------------------------------------
us_states <- rnaturalearth::ne_states(country = "United States of America",
                                      returnclass = "sf") |>
  st_transform(4326)

az <- us_states[us_states$postal == "AZ", ]

# -------------------------------------------------------------
# 4. Load species occurrence data
# -------------------------------------------------------------
# occurdata.csv lives in the same folder as this script.
species_locations <- read.csv("occurdata.csv")

occurdata_sf <- st_as_sf(species_locations,
                         coords = c("Longitude", "Latitude"),
                         crs    = 4326)

print(st_crs(occurdata_sf))
plot(st_geometry(occurdata_sf), pch = 20, col = "darkgreen",
     main = "Pinus ponderosa occurrence points")

# -------------------------------------------------------------
# 5. Reduce collinearity among bioclim predictors (VIF)
# -------------------------------------------------------------
geographic_extent <- ext(occurdata_sf)
bioc <- crop(bio, geographic_extent)

ex     <- terra::extract(bioc, occurdata_sf)
ex_df  <- as.data.frame(ex)[, -1]   # drop the ID column

v1 <- vifstep(ex_df)
print(v1)

non_collinear_vars <- exclude(bioc, v1)

# Convert SpatRaster -> RasterStack via temp .tif files (sdm wants raster::stack)
temp_files <- vapply(seq_len(nlyr(non_collinear_vars)), function(i) {
  f <- tempfile(fileext = ".tif")
  terra::writeRaster(non_collinear_vars[[i]], f, overwrite = TRUE)
  f
}, character(1))
bioc_stack_raster <- raster::stack(temp_files)

# -------------------------------------------------------------
# 6. Build the SDM (GLM + BRT + RF)
#    Heads up: this can take ~5 minutes on a laptop.
# -------------------------------------------------------------
occurdata_sp <- as(occurdata_sf, "Spatial")

d1 <- sdmData(species ~ ., occurdata_sp,
              predictors = bioc_stack_raster,
              bg = list(method = "gRandom", n = 10000, remove = TRUE))

m1 <- suppressWarnings(
  sdm(species ~ ., d1,
      methods       = c("glm", "brt", "rf"),
      replication   = "cv",
      cv.folds      = 3,
      n             = 1,
      modelSettings = list(
        brt = list(tree.complexity = 3, learning.rate = 0.01),
        rf  = list(ntree = 200)
      ))
)

# -------------------------------------------------------------
# 7. Evaluate the models
# -------------------------------------------------------------
eval1 <- sdm::getEvaluation(m1)
print(eval1)
roc(m1)

# -------------------------------------------------------------
# 8. Build a weighted ensemble (current climate)
# -------------------------------------------------------------
species_name <- "Pinus_ponderosa"

en1 <- sdm::ensemble(
  m1, bioc_stack_raster,
  filename = file.path(cache_dir, paste0(species_name, "_T1.tif")),
  setting  = list(method = "weighted", stat = "tss", opt = 2),
  overwrite = TRUE
)

# -------------------------------------------------------------
# 9. Predictor importance
# -------------------------------------------------------------
print(getVarImp(m1))
plot(getVarImp(m1, method = "rf"),
     main = "Variable importance — Random Forest")

# -------------------------------------------------------------
# 10. Presence/absence map from ensemble + threshold
# -------------------------------------------------------------
df_full <- as.data.frame(d1)
coords_mat <- d1@info@coords
df <- data.frame(
  presabs = df_full$species,
  x = coords_mat[, "coords.x1"],
  y = coords_mat[, "coords.x2"]
)

ev <- sdm::evaluates(df$presabs,
                     terra::extract(terra::rast(en1), df[, c("x", "y")])[, 1])

th <- ev@threshold_based$threshold[2]

pa1 <- terra::rast(en1)
pa1[] <- ifelse(en1[] >= th, 1, 0)

terra::writeRaster(pa1,
                   file.path(cache_dir, "Pinus_ponderosa_current_PA.tif"),
                   overwrite = TRUE)

# -------------------------------------------------------------
# 11. Future ensemble (SSP 5-8.5, 2041-2060)
# -------------------------------------------------------------
biof6180c <- crop(biof6180, geographic_extent)

enbiof6180 <- sdm::ensemble(
  m1, biof6180c,
  filename = file.path(cache_dir, paste0(species_name, "_T2.tif")),
  setting  = list(method = "weighted", stat = "tss", opt = 2),
  overwrite = TRUE
)

# -------------------------------------------------------------
# 12. Side-by-side maps with Flagstaff highlighted
# -------------------------------------------------------------

# --- Convert ensembles to SpatRaster FIRST (these were used before being defined) ---
en1_t        <- if (inherits(en1, "SpatRaster"))        en1        else terra::rast(en1)
enbiof6180_t <- if (inherits(enbiof6180, "SpatRaster")) enbiof6180 else terra::rast(enbiof6180)
az_v         <- terra::vect(az)

# --- Build threshold + binary rasters ---
if (exists("th")) rm(th)
df_full    <- as.data.frame(d1)
coords_mat <- d1@info@coords
xy_cols <- intersect(c("coords.x1", "x", "X"), colnames(coords_mat))[1]
yx_cols <- intersect(c("coords.x2", "y", "Y"), colnames(coords_mat))[1]
df <- data.frame(presabs = df_full$species,
                 x = coords_mat[, xy_cols],
                 y = coords_mat[, yx_cols])

pred_vals <- terra::extract(en1_t, df[, c("x", "y")])[, 2]
ev <- sdm::evaluates(df$presabs, pred_vals)
tb <- ev@threshold_based
th <- tb$threshold[tb$criteria == "max(se+sp)"]
cat("th =", th, "\n")

rcl <- matrix(c(-Inf, th, 0,
                th,  Inf, 1), ncol = 3, byrow = TRUE)
pa_current <- terra::classify(en1_t,        rcl)
pa_future  <- terra::classify(enbiof6180_t, rcl)

pa_current_crop <- terra::mask(pa_current, az_v)
pa_future_crop  <- terra::mask(pa_future,  az_v)

cat("pa_current_crop exists:", exists("pa_current_crop"), "\n")
terra::freq(pa_current_crop)

# --- Flagstaff marker helper ---
flagstaff_coords <- c(-111.6513, 35.1983)
mark_flagstaff <- function() {
  points(flagstaff_coords[1], flagstaff_coords[2],
         pch = 1, cex = 1.6, lwd = 2, col = "black")
  points(flagstaff_coords[1], flagstaff_coords[2],
         pch = 20, cex = 0.4, col = "black")
  text(flagstaff_coords[1] + 0.3, flagstaff_coords[2] + 0.4,
       "Flagstaff", pos = 4, cex = 0.8, font = 2)
}

xlim <- c(-117, -105)
ylim <- c(30, 40)
hab_palette <- colorRampPalette(
  c("#3E49BB", "#3498DB", "yellow", "orange", "red", "darkred")
)(200)

# --- Mask suitability rasters to AZ ---
en1_crop        <- terra::mask(en1_t,        az_v)
enbiof6180_crop <- terra::mask(enbiof6180_t, az_v)

# --- Suitability maps ---
par(mfrow = c(1, 2), mar = c(2, 2, 3, 2), oma = c(1, 1, 1, 1))
terra::plot(en1_crop, xlim = xlim, ylim = ylim, range = c(0, 1),
            main = "Current Ponderosa habitat suitability",
            col  = hab_palette, axes = TRUE)
plot(st_geometry(az), add = TRUE, border = "grey30", lwd = 0.6)
mark_flagstaff()

terra::plot(enbiof6180_crop, xlim = xlim, ylim = ylim, range = c(0, 1),
            main = "Future Ponderosa habitat (SSP 5-8.5, 2041-2060)",
            col  = hab_palette, axes = TRUE)
plot(st_geometry(az), add = TRUE, border = "grey30", lwd = 0.6)
mark_flagstaff()

# --- Presence/absence maps ---
pa_palette <- c("grey85", "darkgreen")
par(mfrow = c(1, 2), mar = c(2, 2, 3, 2), oma = c(1, 1, 1, 1))
terra::plot(pa_current_crop, xlim = xlim, ylim = ylim,
            main = paste0("Current predicted presence (th = ", round(th, 3), ")"),
            col = pa_palette, axes = TRUE, legend = FALSE)
plot(st_geometry(az), add = TRUE, border = "grey30", lwd = 0.6)
mark_flagstaff()

terra::plot(pa_future_crop, xlim = xlim, ylim = ylim,
            main = "Future predicted presence (SSP 5-8.5, 2041-2060)",
            col = pa_palette, axes = TRUE, legend = FALSE)
plot(st_geometry(az), add = TRUE, border = "grey30", lwd = 0.6)
mark_flagstaff()
par(mfrow = c(1, 1))

# --------------------------------------------
# End of script
# --------------------------------------------