# Purpose: threshold water rasters, turning continuous model output into binary water/notwater
#
# Created: Apr 2016 (nelliott)
# Last modified: Apr 2016 (nelliott)

# Depends on most other code files in autowater\functions

# Main function for stacking the rasters
thresholdWaterRasters <- function(baseDir, scenes, dateStart, dateEnd, tempDir = tempdir(), 
                            overwrite = FALSE, logOutLevel = "TRACE", waterThreshold = 0.1552574) {
  
  withConditionLogging({
    
    # Report
    raiseCondition(message_debug_enter())
    
    # Check inputs
    checkInput(c("baseDir", "scenes", "dateStart", "dateEnd", "tempDir", "logOutLevel", "waterThreshold"),
               classes = c("character", "character", "character", "character", "character", "character", "numeric"),
               lengths = c(1, NA, 1, 1, 1, 1, 1))
    
    # Base directory
    checkFile(baseDir)
    
    # Temp directory
    checkFile(tempDir)
    
    # Buckets
    pvtBkt <- "gs://pointblue-autowater-pvt"
    pvtDir <- file.path(baseDir, "pvt")

    prdBkt <- file.path(pvtBkt, "predicted/continuous")
    prdBktMnt <- file.path(pvtDir, "predicted/continuous")
    
    thrBkt <- file.path(pvtBkt, "predicted/thresholded")
    thrBktMnt <- file.path(pvtDir, "predicted/thresholded")
    
    # Check that the drives are mounted, runing mounting script if not
    mounted <- gsCheck(condClass = "error", mount = TRUE)
    
    # Check directories
    checkFile(c(tempDir, prdBktMnt, thrBktMnt))
    
    # Raster temp dir
    library(raster)
    options(rasterTmpDir = tempDir)

    # Get matching files
    files <- listFilesBySceneDate(prdBktMnt, sceneChars = c(4, 9), scenes = scenes, 
                                  dateChars = c(11, 18), dateFormat = "%Y%m%d", dateStart = dateStart, dateEnd = dateEnd, 
                                  searchPattern = "watercont.tif$", full.names = TRUE)
    
    # Iterate outough files
    for (f in files) {
      
      raiseCondition(message_info(paste("Working on prediction for file", f)))
      
      # Out filename
      thrFn <- gsub("watercont", "water", basename(f))
      thrFile <- file.path(tempDir, thrFn)
      thrFileBkt <- file.path(thrBkt, thrFn)
      thrFileBktMnt <- file.path(thrBktMnt, thrFn)
      
      # Check if output file exists
      if (file.exists(thrFileBktMnt)) {
        
        if (overwrite == TRUE) {
          
          raiseCondition(message_info(paste("Overwriting previous output (overwrite set to TRUE)")))
          
        } else {
          
          raiseCondition(message_info(paste("Image", f, "has already been thresholded and overwrite = FALSE.",
                                            "Moving to next...")))
          next
          
        }
        
      }
      
      # Load raster
      prdRst <- raster(f)
      
      # Threshold
      raiseCondition(message_trace("Thresholding..."))
      thrRst <- reclassify(prdRst, rcl = c(0, waterThreshold, 0, waterThreshold, 1, 1), include.lowest = TRUE,
                           filename = thrFile, overwrite = TRUE)
      writeSemaphore(thrFile)
      message(message_info("Raster successfully thresholded."))
      
      # Copy file to cloud, deleting original
      gsCopy(thrFile, thrFileBkt, copySem = TRUE, deleteFrom = TRUE)
      
    } #close file iteration
    
  }, logOutLevel = logOutLevel) #close logging
  
} #close master function
