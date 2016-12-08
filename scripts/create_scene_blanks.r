# Script to create blanks for each scene that are used when mosaicing and there's no image

# Directories
baseDir <- "/home/blue"
codeDir <- file.path(baseDir, "/sparklemotion/autowater/functions")
tempDir <- tempdir()

pvtBkt <- "gs://pointblue-autowater-pvt"
pvtDir <- file.path(baseDir, "pvt")

# Load code
source(file.path(codeDir, "loading_code_independent.r"))
allSourced <- sourceDir(codeDir)
if (!allSourced) stop(paste0("Could not successfully load all code in ", codeDir, ". Exiting."))

# Packages
library(rgdal)
library(raster)

# Set directories
refDir <- file.path(pvtDir, "reference")
scnDir <- file.path(refDir, "scene_extents")
blkDir <- file.path(refDir, "blanks")

# Load ref rasters
files <- list.files(scnDir, pattern = "crop_extent_small.tif$", full.name = TRUE)

# Turn to NAs and write
for (f in files) {
  
  rst <- raster(f)
  rst[] <- NA
  
  outFn <- gsub("crop_extent_small", "blank", basename(f))
  writeRaster(rst, filename = file.path(blkDir, outFn))
    
}

# Project p42r35 blank into utm 10
inFile <- file.path(blkDir, "p42r35_crop_extent_small.tif")
inRst <- raster(inFile)
outFile <- gsub("small", "small_utm10", inFile)
outCRS <- "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
outRst <- projectRaster(from = inRst, crs = outCRS, method = "ngb", filename = outFile)



