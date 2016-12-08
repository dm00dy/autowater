# Code to snap cv extent rasters for processing

# Packages
library(rgdal)
library(raster)

# Directories
baseDir <- "/home/blue"
codeDir <- file.path(baseDir, "/sparklemotion/autowater/functions")
tempDir <- file.path(baseDir, "processing")

pvtBkt <- "gs://pointblue-autowater-pvt"
pvtBktMnt <- file.path(baseDir, "pvt")

refBkt <- file.path(pvtBkt, "reference/cv_extents")
refBktMnt <- file.path(pvtBktMnt, "reference/cv_extents")

# Load code
source(file.path(codeDir, "loading_code_independent.r"))
allSourced <- sourceDir(codeDir)
if (!allSourced) stop(paste0("Could not successfully load all code in ", codeDir, ". Exiting."))

withConditionLogging({

# Check that the drives are mounted, runing mounting script if not
mounted <- gsCheck(condClass = "error", mount = TRUE)


# Loop across scenes
scenes <- c("p44r34", "p44r33", "p43r34", "p42r35")
scenes <- c("p43r34", "p42r35")
scenes <- "p44r33"
for (scn in scenes) {
  
  message(paste("Working on scene", scn))
  
  # Get files
  cvFile <- file.path(refBktMnt, paste0("cvjv_", scn, "_offset.tif"))
  snapFile <- file.path(pvtBktMnt, "reference/scene_extents", paste0(scn, "_crop_extent_small.tif"))
  outFile <- file.path(tempDir, paste0("cvjv_", scn, ".tif"))
  outFileBkt <- file.path(refBktMnt, paste0("cvjv_", scn, ".tif"))
  
  # Snap extent raster
  snapRaster(cvFile, snapFile, outFile, overwrite = FALSE, tempDir = tempDir)
  
  # Write to cloud
  gsCopy(outFile, outFileBkt, copySem = TRUE, deleteFrom = TRUE)
  
}

}, logOutLevel = "TRACE")

