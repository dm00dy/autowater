# Purpose: predict surface water by applying water model to landsat rasters
#
# Created: Apr 2016 (nelliott)
# Last modified: Apr 2016 (nelliott)

# Depends on most other code files in autowater\functions

# Main function for stacking the rasters
predictWaterRasters <- function(baseDir, scenes, dateStart, dateEnd, tempDir = tempdir(), 
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

    stkBkt <- file.path(pvtBkt, "stacked")
    stkBktMnt <- file.path(pvtDir, "stacked")
    
    mskBkt <- file.path(stkBkt, "masked")
    mskBktMnt <- file.path(stkBktMnt, "masked")
    
    outBkt <- file.path(pvtBkt, "predicted/continuous")
    outBktMnt <- file.path(pvtDir, "predicted/continuous")
    
    # Check that the drives are mounted, runing mounting script if not
    mounted <- gsCheck(condClass = "error", mount = TRUE)
    
    # Check directories
    checkFile(c(tempDir, stkBktMnt, mskBktMnt))
    
    # Raster temp dir
    library(raster)
    options(rasterTmpDir = tempDir)
    
    # Load packages for predicting
    library(gbm)
    library(dismo)
    
    # Load model
    mdlFile <- file.path(pvtDir, "/model/landsat8_brt.RData")
    checkFile(mdlFile)
    load(mdlFile)
    
    # Load metadata
    mdDf <- read.csv(file.path(pvtDir, "metadata/landsat_metadata.csv"))
    
    # Get matching files
    files <- listFilesBySceneDate(stkBktMnt, sceneChars = c(4, 9), scenes = scenes, 
                                  dateChars = c(11, 18), dateFormat = "%Y%m%d", dateStart = dateStart, dateEnd = dateEnd, 
                                  searchPattern = "stacked.tif$", full.names = TRUE)
    
    # Iterate through files
    for (f in files) {
      
      raiseCondition(message_info(paste("Working on prediction for file", f)))
      
      # Out filename
      outFn <- gsub("stacked", "watercont", basename(f))
      outFile <- file.path(tempDir, outFn)
      outFileBkt <- file.path(outBkt, outFn)
      outFileBktMnt <- file.path(outBktMnt, outFn)
      
      # Check if output file exists
      if (file.exists(outFileBktMnt)) {
        
        if (overwrite == TRUE) {
          
          raiseCondition(message_info(paste("Overwriting previous output (overwrite set to TRUE)")))
          
        } else {
          
          raiseCondition(message_info(paste("Image", f, "has already been predicted to and overwrite = FALSE.",
                                            "Moving to next...")))
          next
          
        }
        
      }
      
      
      message(message_trace(paste0("Checking cloud mask for file ", f, "...")))
      
      # Parse filename
      fnDf <- parseFilenamePB(f)
      pth <- fnDf$Path
      rw <- fnDf$Row
      dt <- as.Date(fnDf$Date)
      
      # Get cloud cover of image from metadata
      cloudCover <- mdDf$CLOUD_COVER[mdDf$WRS_PATH == pth & mdDf$WRS_ROW == rw & as.Date(mdDf$DATE_ACQUIRED) == dt]
      message(message_debug_vector("Cloud cover", cloudCover))
      
      # Check against cloud cover threshold
      if (cloudCover < cloudCoverThreshold) {
        
        message(message_trace(paste0("Cloud cover of image (", cloudCover, 
                                     "%) is less than the minimum  to use cloud-masked file. Loading unmasked file...")))
        stkFile <- f
        
      # Otherwise, apply mask
      } else {
        
        message(message_trace(paste0("Cloud cover of image (", cloudCover, 
                                     "%) is more than the minimum to use cloud-masked file. Loading masked file...")))
        stkFile <- file.path(mskBktMnt, gsub("stacked", "masked", basename(f)))
        
        fileValid <- checkFile(stkFile, "warning")
        
        if (fileValid == FALSE) {

          raiseCondition(file_missing("Cloud-masked file missing, likely because it was too cloudy. Skipping...", "warning"))
          next
        
        }
        
      }
      
      # Load raster stack
      raiseCondition(message_debug_vector("Raster stack file", stkFile))
      stk <- stack(stkFile)
      
      # Assign names
      names(stk) <- c(paste0("Band", c(1:7, 9:11)), "NDWI", "MNDWI")

      # Predict
      raiseCondition(message_trace("Predicting..."))
      outRst <- predict(stk, model = brt, type = "response", n.trees = brt$n.trees, filename = outFile, overwrite = TRUE)
      writeSemaphore(outFile)
      message(message_info("Raster successfully predicted."))
      
      # Copy file to cloud, deleting original
      gsCopy(outFile, outFileBkt, copySem = TRUE, deleteFrom = TRUE)
      
    } #close file iteration
    
  }, logOutLevel = logOutLevel) #close logging
  
} #close master function
