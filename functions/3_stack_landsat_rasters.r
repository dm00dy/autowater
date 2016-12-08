# Purpose: create a stack of the desired landsat rasters from each scene
#
# Created: Apr 2016 (nelliott)
# Last modified: Apr 2016 (nelliott)

# Depends on most other code files in autowater\functions


# Main function for stacking the rasters
stackLandsatRasters <- function(baseDir, scenes, dateStart, dateEnd, tempDir = tempdir(), logOutLevel = "TRACE", overwrite = FALSE) {
  
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
    
    # Set directories
    #baseDir <- "/home/blue"
    #codeDir <- file.path(baseDir, "sparklemotion/autowater/functions")
    
    # Buckets
    pvtBkt <- "gs://pointblue-autowater-pvt"
    snapBkt <- file.path(pvtBkt, "snapped")
    snapBktMnt <- file.path(baseDir, "pvt/snapped")
    ndiBkt <- file.path(pvtBkt, "pvt/ndi")
    ndiBktMnt <- file.path(baseDir, "pvt/ndi")
    stkBkt <- file.path(pvtBkt, "stacked")
    stkBktMnt <- file.path(baseDir, "pvt/stacked")
    
    # Check that the drives are mounted, runing mounting script if not
    mounted <- gsCheck(condClass = "error", mount = TRUE)
    
    # Check directories
    checkFile(c(tempDir, snapBktMnt, ndiBktMnt, stkBktMnt))
    
    # Raster temp dir
    library(raster)
    options(rasterTmpDir = tempDir)
    
    # Load sourcing code
    #source(file.path(codeDir, "loading_code_independent.r"))
    
    # Source code files
    #sourceDir(codeDir)
    
    # Desired bands and indexes
    wantedBands <- paste0("B", c(1:7, 9:11))
    wantedIndexes <- c("ndwi", "mndwi")
  
    # Get matching files
    files <- listFilesBySceneDate(snapBktMnt, sceneChars = c(4, 9), scenes = scenes, 
                                  dateChars = c(11, 18), dateFormat = "%Y%m%d", dateStart = dateStart, dateEnd = dateEnd, 
                                  searchPattern = "tif$")
    
    baseFiles <- unique(substr(files, 1, 18))
    raiseCondition(message_info(paste("Found", length(baseFiles), "unique images to stack.")))
    
    # Iterate through files
    for (bf in baseFiles) {
      
      message(message_info(paste0("Stacking images for ", bf, "...")))
      
      # Check the necessary files exist
      bandFiles <- file.path(snapBktMnt, paste0(bf, "_", wantedBands, "_snapped_v00.tif"))
      message(message_debug_vector("Matching band files:", bandFiles))
      
      ndiFiles <- file.path(ndiBktMnt, paste0(bf, "_", wantedIndexes, "_v00.tif"))
      message(message_debug_vector("Matching ndi files:", ndiFiles))
      
      # Combine and check files
      inFiles <- c(bandFiles, ndiFiles)
      checkFile(inFiles)
      
      # Out file name
      outFn <- paste0(bf, "_stacked.tif")
      outFile <- file.path(tempDir, outFn)
      outFileBkt <- file.path(stkBkt, outFn)
      outFileBktMnt <- file.path(stkBktMnt, outFn)
      
      # Check if output file exists
      if (file.exists(outFileBktMnt)) {
        
        if (overwrite == TRUE) {
          
          raiseCondition(message_info(paste("Overwriting previous output (overwrite set to TRUE)")))
          
        } else {
          
          raiseCondition(message_info(paste("Image", bf, "has already been stacked and overwrite = FALSE.",
                                            "Moving to next...")))
          next
          
        }
        
      }
      
      # Stack
      message(message_trace(paste0("Stacking rasters for ", bf, "...")))
      stk <- stack(inFiles)
      names(stk) <- c(wantedBands, wantedIndexes)
      writeRaster(stk, filename = outFile, overwrite = TRUE)
      writeSemaphore(outFile)
      message(message_info(paste("Rasters for ", bf, "successfully stacked.")))
      
      # Copy reclassified file to cloud, deleting original
      gsCopy(outFile, outFileBkt, copySem = TRUE, deleteFrom = TRUE)
      
    }  
    
  }, logOutLevel = logOutLevel)
  
}

