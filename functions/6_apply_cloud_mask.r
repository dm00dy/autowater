# Purpose: apply cloud masks to landsat rasters
#
# Created: Apr 2016 (nelliott)
# Last modified: Apr 2016 (nelliott)

# Depends on most other code files in autowater\functions

# Main function for stacking the rasters
applyCloudMasks <- function(baseDir, scenes, dateStart, dateEnd, tempDir = tempdir(), 
                            overwrite = FALSE, logOutLevel = "TRACE", cloudCoverThreshold = 5) {
  
  withConditionLogging({
    
    # Report
    raiseCondition(message_debug_enter())
    
    # Check inputs
    checkInput(c("baseDir", "scenes", "dateStart", "dateEnd", "tempDir", "logOutLevel"),
               classes = c("character", "character", "character", "character", "character", "character"),
               lengths = c(1, NA, 1, 1, 1, 1))
    
    # Base directory
    checkFile(baseDir)
    
    # Temp directory
    checkFile(tempDir)
    
    # Buckets
    pvtBkt <- "gs://pointblue-autowater-pvt"
    pvtDir <- file.path(baseDir, "pvt")
    
    cloudBkt <- file.path(pvtBkt, "cloud_mask")
    bufBkt <- file.path(cloudBkt, "buffered")
    bufBktMnt <- file.path(pvtDir, "cloud_mask/buffered")

    stkBkt <- file.path(pvtBkt, "stacked")
    stkBktMnt <- file.path(pvtDir, "stacked")
    
    outBkt <- file.path(stkBkt, "masked")
    outBktMnt <- file.path(stkBktMnt, "masked")
    
    # Check that the drives are mounted, runing mounting script if not
    mounted <- gsCheck(condClass = "error", mount = TRUE)
    
    # Check directories
    checkFile(c(tempDir, bufBktMnt, stkBktMnt, outBktMnt))
    
    # Raster temp dir
    library(raster)
    options(rasterTmpDir = tempDir)

    # Get matching files
    files <- listFilesBySceneDate(stkBktMnt, sceneChars = c(4, 9), scenes = scenes, 
                                  dateChars = c(11, 18), dateFormat = "%Y%m%d", dateStart = dateStart, dateEnd = dateEnd, 
                                  searchPattern = "stacked.tif$", full.names = TRUE)
    
    # Iterate through files
    for (f in files) {
      
      message(message_info(paste0("Checking cloud mask for file ", f, "...")))
      
      # Parse filename
      fnDf <- parseFilenamePB(f)
      pth <- fnDf$Path
      rw <- fnDf$Row
      dt <- as.Date(fnDf$Date)
      
      # Get cloud cover of image from metadata
      mdDf <- read.csv(file.path(pvtDir, "metadata/landsat_metadata.csv"))
      cloudCover <- mdDf$CLOUD_COVER[mdDf$WRS_PATH == pth & mdDf$WRS_ROW == rw & as.Date(mdDf$DATE_ACQUIRED) == dt]
      message(message_debug_vector("Cloud cover", cloudCover))
      
      # Check against cloud cover threshold
      if (cloudCover < cloudCoverThreshold) {
        
        message(message_info(paste0("Cloud cover of image (", cloudCover, 
                                    "%) is less than the minimum specified to apply mask. Moving to next...")))
        
      # Otherwise, apply mask
      } else {
        
        message(message_trace(paste0("Cloud cover of image (", cloudCover, 
                                        "%) is more than the minimum specified to apply mask.")))
        
        # Out filename
        outFn <- gsub("stacked", "masked", basename(f))
        outFile <- file.path(tempDir, outFn)
        outFileBkt <- file.path(outBkt, outFn)
        outFileBktMnt <- file.path(outBktMnt, outFn)
        
        # Check if output file exists
        if (file.exists(outFileBktMnt)) {
          
          if (overwrite == TRUE) {
            
            raiseCondition(message_info(paste("Overwriting previous output (overwrite set to TRUE)")))
            
          } else {
            
            raiseCondition(message_info(paste("Image", f, "has already been cloud masked and overwrite = FALSE.",
                                              "Moving to next...")))
            next
            
          }
          
        }
        
        # Load rasters
        stk <- stack(f)
        maskFile <- file.path(bufBktMnt, createFilenamePB(fnDf, newType = "clouds", newVersion = "00"))
        checkFile(maskFile)
        maskRst <- raster(maskFile)
        
        # Stop if no cloud-free areas
        message(paste("Mask Raster min value:", maskRst@data@min))
        print(maskRst)
        if (maskRst@data@min == 1) {
          
          # Add to metadata
          message(message_info("Deleting bad files."))
          
          # Delete later files
          toDelete <- c(list.files(outBktMnt, pattern = substr(basename(f), 1, 18), full.names = TRUE),
                        list.files(file.path(pvtDir, "predicted/continuous"), pattern = substr(basename(f), 1, 18), full.names = TRUE),
                        list.files(file.path(pvtDir, "predicted/thresholded"), pattern = substr(basename(f), 1, 18), full.names = TRUE),
                        list.files("/home/blue/pub/single_scenes", pattern = substr(basename(f), 1, 18), full.names = TRUE))
          file.remove(toDelete)
          
          # Skip
          raiseCondition(raster_cloudmask_failure("No non-clouded areas of image exist. Skipping.", "warning"))
          next
          
        } else {
          
          # Mask
          raiseCondition(message_trace("Masking..."))
          tryCatch({
            outRst <- mask(stk, maskRst, maskvalue = 1, filename = outFile, overwrite = TRUE)
            writeSemaphore(outFile)
            message(message_info("Raster successfully masked."))
          })
          #}, warning = function(w) {
          #  invokeRestart("muffleWarning")
          #})
          
          # Copy summarized file to cloud, deleting original
          gsCopy(outFile, outFileBkt, copySem = TRUE, deleteFrom = TRUE)
          
        }
        
      }
          
    }  
    
  }, logOutLevel = logOutLevel, logFileLevel = "FATAL")
  
}
