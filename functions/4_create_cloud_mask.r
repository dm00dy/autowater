# Purpose: summarize landsat cloud rasters
#
# Created: Apr 2016 (nelliott)
# Last modified: Apr 2016 (nelliott)

# Depends on most other code files in autowater\functions


# Main function for stacking the rasters
createCloudMasks <- function(baseDir, scenes, dateStart, dateEnd, tempDir = tempdir(), logOutLevel = "TRACE", overwrite = FALSE) {
  
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
    
    snapBkt <- file.path(pvtBkt, "snapped")
    snapBktMnt <- file.path(baseDir, "pvt/snapped")
    
    unbufBkt <- file.path(pvtBkt, "cloud_mask/unbuffered")
    unbufBktMnt <- file.path(baseDir, "pvt/cloud_mask/unbuffered")
    
    # Check that the drives are mounted, runing mounting script if not
    mounted <- gsCheck(condClass = "error", mount = TRUE)
    
    # Check directories
    checkFile(c(tempDir, snapBktMnt, unbufBktMnt))
    
    # Raster temp dir
    library(raster)
    options(rasterTmpDir = tempDir)

    # Get matching files
    files <- listFilesBySceneDate(snapBktMnt, sceneChars = c(4, 9), scenes = scenes, 
                                  dateChars = c(11, 18), dateFormat = "%Y%m%d", dateStart = dateStart, dateEnd = dateEnd, 
                                  searchPattern = "_cfmask_.*tif$", full.names = TRUE)
    
    # Iterate through files
    for (f in files) {
      
      message(message_info(paste0("Extracting cloud mask from file ", f, "...")))

      # Out filename
      fnDf <- parseFilenamePB(f)
      outFn <- gsub("cfmask_snapped", "clouds", basename(f))
      outFile <- file.path(tempDir, outFn)
      outFileBkt <- file.path(unbufBkt, outFn)
      outFileBktMnt <- file.path(unbufBktMnt, outFn)
      
      # Check if output file exists
      if (file.exists(outFileBktMnt)) {
        
        if (overwrite == TRUE) {
          
          raiseCondition(message_info(paste("Overwriting previous output (overwrite set to TRUE)")))
          
        } else {
          
          raiseCondition(message_info(paste("Cloud mask for image", f, "has already been created and overwrite = FALSE.",
                                            "Moving to next...")))
          next
          
        }
        
      }
      
      # Create cloud mask
      cloudMaskFromCF(f, outFile, overwrite = overwrite, ncores = 1, tempDir = tempDir, condClass = "error")

      # Copy summarized file to cloud, deleting original
      gsCopy(outFile, outFileBkt, copySem = TRUE, deleteFrom = TRUE)
      
    }  
    
  }, logOutLevel = logOutLevel)
  
}
