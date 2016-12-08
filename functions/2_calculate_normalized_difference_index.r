# Purpose: calculate normalized difference index between two specified bands for all files in the date range
# Uses the formula (A - B) / (A + B)
#
# Created: Apr 2016 (nelliott)
# Last modified: Apr 2016 (nelliott)

# Depends on most other code files in autowater\functions

# THINGS TO DO:

# Wrapper for normalized difference vegetation index (NDVI)
calculateNDVI <- function(baseDir, scenes, dateStart, dateEnd, tempDir = tempdir(), logOutLevel = "TRACE", overwrite = FALSE) {
  
  calculateNormalizedDifferenceIndex(index = "ndvi", bandA = 5, bandB = 4, 
                                     baseDir = baseDir, scenes = scenes, dateStart = dateStart, dateEnd = dateEnd, 
                                     tempDir = tempDir, logOutLevel = logOutLevel, overwrite = overwrite)
  
}

# Wrapper for normalized difference water index (NDWI)
calculateNDWI <- function(baseDir, scenes, dateStart, dateEnd, tempDir = tempdir(), logOutLevel = "TRACE", overwrite = FALSE) {
  
  calculateNormalizedDifferenceIndex(index = "ndwi", bandA = 3, bandB = 5, 
                                     baseDir = baseDir, scenes = scenes, dateStart = dateStart, dateEnd = dateEnd, 
                                     tempDir = tempDir, logOutLevel = logOutLevel, overwrite = overwrite)
  
}

# Wrapper for modified normalized difference water index (MNDWI)
calculateMNDWI <- function(baseDir, scenes, dateStart, dateEnd, tempDir = tempdir(), logOutLevel = "TRACE", overwrite = FALSE) {
  
  calculateNormalizedDifferenceIndex(index = "mndwi", bandA = 3, bandB = 6, 
                                     baseDir = baseDir, scenes = scenes, dateStart = dateStart, dateEnd = dateEnd, 
                                                 tempDir = tempDir, logOutLevel = logOutLevel, overwrite = overwrite)
  
}

# Main function for calculating normalized difference index
calculateNormalizedDifferenceIndex <- function(index, bandA, bandB, baseDir, scenes, dateStart, dateEnd, 
                                               tempDir = tempdir(), logOutLevel = "TRACE", overwrite = FALSE) {
  
  withConditionLogging({
    
    # Report
    raiseCondition(message_debug_enter())
    
    # Check inputs
    checkInput(c("index", "bandA", "bandB", "baseDir", "scenes", "dateStart", "dateEnd", "tempDir", "logOutLevel"),
               classes = c("character", "numeric", "numeric", "character", "character", "character", "character", "character", "character"),
               lengths = c(1, 1, 1, 1, NA, 1, 1, 1, 1))
    
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
    snapBktDir <- file.path(baseDir, "pvt/snapped")
    ndiBkt <- file.path(pvtBkt, "ndi")
    ndiBktMnt <- file.path(baseDir, "pvt/ndi")
    
    # Check that the drives are mounted, runing mounting script if not
    mounted <- gsCheck(condClass = "error", mount = TRUE)
    
    # Check directories
    checkFile(c(tempDir, snapBktDir, ndiBktMnt))
    
    # Raster temp dir
    library(raster)
    options(rasterTmpDir = tempDir)
    
    # Load sourcing code
    #source(file.path(codeDir, "loading_code_independent.r"))
    
    # Source code files
    #sourceDir(codeDir)
  
    # Get matching folders
    files <- listFilesBySceneDate(snapBktDir, sceneChars = c(4, 9), scenes = scenes, 
                                  dateChars = c(11, 18), dateFormat = "%Y%m%d", dateStart = dateStart, dateEnd = dateEnd,
                                  searchPattern = paste0("B", bandA, ".*tif$"), full.names = TRUE)
  
    # Iterate through files
    for (fa in files) {
      
      message(message_info(paste0("Calculating ", index, " for file ", fa, "...")))
      
      # Check there's a matching band file
      fb <- gsub(paste0("_B", bandA, "_"), paste0("_B", bandB, "_"), fa)
      raiseCondition(message_debug(paste("Matching band", bandB, "file:", fb)))
      if (!file.exists(fb)) {
        
        raiseCondition(file_missing(paste0("matching file for band ", bandB, ": ", fb, ". Skipping to next..."), 
                                    "warning", file = fb))
        next
        
      }
      
      # Name
      fnDf <- parseFilenamePB(fa)
      fnDf$Band <- index
      outFn <- createFilenamePB(fnDf, newType = "", newVersion = "00")
      outFile <- file.path(tempDir, outFn)
      outFileBkt <- file.path(ndiBkt, outFn)
      outFileBktMnt <- file.path(ndiBktMnt, outFn)
      
      # Check if output file exists
      if (file.exists(outFileBktMnt)) {

        if (overwrite == TRUE) {
          
          raiseCondition(message_info(paste("Overwriting previous output (overwrite set to TRUE)")))
          
        } else {
          
          raiseCondition(message_info(paste("Difference index for file", basename(fa), "already processed and overwrite = FALSE.",
                                              "Moving to next.")))
          next
          
        }
        
      }
      
      # Calculate NDI
      message(message_trace(paste0("Calculating ", index, " using files ", fa, " and ", fb, "...")))
      calcNormDiff(fa, fb, outFile, overwrite = FALSE, condClass = "error")
      message(message_info(paste(index, "raster ", basename(outFile), "successfully calculated.")))
      
      # Copy reclassified file to cloud, deleting original
      gsCopy(outFile, outFileBkt, copySem = TRUE, deleteFrom = TRUE)
      
    }  
    
  }, logOutLevel = logOutLevel)
  
}

