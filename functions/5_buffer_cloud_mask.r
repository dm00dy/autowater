# Purpose: summarize landsat cloud rasters
#
# Created: Apr 2016 (nelliott)
# Last modified: Apr 2016 (nelliott)

# Depends on most other code files in autowater\functions


# Main function for stacking the rasters
bufferCloudMasks <- function(baseDir, scenes, dateStart, dateEnd, tempDir = tempdir(), logOutLevel = "TRACE", overwrite = FALSE) {
  
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
    cloudBkt <- file.path(pvtBkt, "cloud_mask")
    unbufBkt <- file.path(cloudBkt, "unbuffered")
    unbufBktMnt <- file.path(baseDir, "pvt/cloud_mask/unbuffered")
    bufBkt <- file.path(cloudBkt, "buffered")
    bufBktMnt <- file.path(baseDir, "pvt/cloud_mask/buffered")
    falseBktMnt <- file.path(baseDir, "pvt/cloud_mask/false_clouds")
    
    # Check that the drives are mounted, runing mounting script if not
    mounted <- gsCheck(condClass = "error", mount = TRUE)
    
    # Check directories
    checkFile(c(tempDir, unbufBktMnt, bufBktMnt))
    
    # Raster temp dir
    library(raster)
    options(rasterTmpDir = tempDir)

    # Get matching files
    files <- listFilesBySceneDate(unbufBktMnt, sceneChars = c(4, 9), scenes = scenes, 
                                  dateChars = c(11, 18), dateFormat = "%Y%m%d", dateStart = dateStart, dateEnd = dateEnd, 
                                  searchPattern = "tif$", full.names = TRUE)
    
    # Iterate through files
    for (f in files) {
      
      message(message_info(paste0("Processing cloud mask for file ", f, "...")))

      # Out filename
      fnDf <- parseFilenamePB(f)
      outFn <- basename(f)
      outFile <- file.path(tempDir, outFn)
      outFileBkt <- file.path(bufBkt, outFn)
      outFileBktMnt <- file.path(bufBktMnt, outFn)
      
      # Check if output file exists
      if (file.exists(outFileBktMnt)) {
        
        if (overwrite == TRUE) {
          
          raiseCondition(message_info(paste("Overwriting previous output (overwrite set to TRUE)")))
          
        } else {
          
          raiseCondition(message_info(paste("Cloud mask for image", f, "has already been buffered and overwrite = FALSE.",
                                            "Moving to next...")))
          next
          
        }
        
      }
      
      # Rasters
      unbufRst <- raster(f)
      falseCloudRst <- raster(file.path(falseBktMnt, paste0(fnDf$Satellite, "_", fnDf$Scene, "_false_clouds.tif")))
      
      # Remove false clouds
      raiseCondition(message_trace("Removing false clouds..."))
      mskRst <- mask(unbufRst, falseCloudRst, maskvalue = 1, updatevalue = 0)
      
      # Buffer
      raiseCondition(message_trace("Buffering..."))
      bufRst <- bufferSimple(mskRst, width = 67) #width in 30m pixels; 67 give 2km buffer, enough for clouds
      writeRaster(bufRst, filename = outFile, overwrite = TRUE)
      writeSemaphore(outFile)
      message(message_info("Buffered cloud mask successfully created."))
      
      # Copy summarized file to cloud, deleting original
      gsCopy(outFile, outFileBkt, copySem = TRUE, deleteFrom = TRUE)
      
    }  
    
  }, logOutLevel = logOutLevel)
  
}
