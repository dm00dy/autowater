# Purpose: summarize landsat cloud rasters
#
# Created: Apr 2016 (nelliott)
# Last modified: Apr 2016 (nelliott)

# Depends on most other code files in autowater\functions


# Main function for stacking the rasters
summarizeCloudRasters <- function(baseDir, scenes, dateStart, dateEnd, tempDir = tempdir(), logOutLevel = "TRACE",
                                  validFiles = read.csv("/home/blue/pvt/cloud_mask/scene_list_clear.csv")$Filename) {
  
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
    prcDir <- file.path(baseDir, "processing")
    cloudDir <- file.path(prcDir, "cloud_mask")
    
    # Buckets
    pvtBkt <- "gs://pointblue-autowater-pvt"
    cloudBkt <- file.path(pvtBkt, "cloud_mask")
    unbufBkt <- file.path(cloudBkt, "unbuffered")
    unbufBktMnt <- file.path(baseDir, "pvt/cloud_mask/unbuffered")
    sumBkt <- file.path(cloudBkt, "summary")
    sumBktMnt <- file.path(baseDir, "pvt/cloud_mask/summary")
    
    # R
    # Check that the drives are mounted, runing mounting script if not
    mounted <- gsCheck(condClass = "error", mount = TRUE)
    
    # Check directories
    checkFile(c(prcDir, cloudDir, unbufBktMnt, sumBktMnt))
    
    # Raster temp dir
    library(raster)
    options(rasterTmpDir = tempDir)
    
    # Load sourcing code
    #source(file.path(codeDir, "loading_code_independent.r"))
    
    # Source code files
    #sourceDir(codeDir)
    
    # Iterate through scenes
    for (scn in scenes) {
      
      message(message_info(paste0("Processing cloud rasters for scene ", scn, " from ", dateStart, " to ", dateEnd, "...")))
      
      # Get matching files
      files <- listFilesBySceneDate(unbufBktMnt, sceneChars = c(4, 9), scenes = scn, 
                                    dateChars = c(11, 18), dateFormat = "%Y%m%d", dateStart = dateStart, dateEnd = dateEnd, 
                                    searchPattern = "tif$", full.names = TRUE)
      
      # Check if in valid files list
      if (!is.na(validFiles)) {
        
        message(message_trace("Restricting files to those in validFiles"))
        message(message_debug_vector("Valid files (validFiles):", validFiles))
        validFilesStr <- paste0("(", paste(validFiles, collapse = ")|("), ")")
        message(message_debug(paste("Valid files string:", validFilesStr)))
        filesTF <- as.logical(vapply(files, FUN = function(x) {sum(grepl(validFilesStr, x), na.rm = TRUE)}, FUN.VALUE = 1))
        message(message_debug_vector("Files match (filesTF)", filesTF))
        files <- files[filesTF]
        
      }
      
      # Out filename
      outFn <- paste0("L8_", scn, "_", format(as.Date(dateStart), format = "%Y%m%d"), "_", 
                      format(as.Date(dateEnd), format = "%Y%m%d"), "_cloud_summary_clear.tif")
      outFile <- file.path(cloudDir, outFn)
      outFileBkt <- file.path(sumBkt, outFn)
      outFileBktMnt <- file.path(sumBktMnt, outFn)
      
      # Check if output file exists
      if (file.exists(outFileBktMnt)) {
        
        raiseCondition(message_info("Cloud data for this scene and date range already processed. Moving to next..."))
        next
        
      }
      
      # Stack and sum
      message(message_trace("Stacking cloud rasters..."))
      stk <- stack(files)
      message(message_trace("Calculating sum..."))
      sumRst <- calc(stk, fun = sum, na.rm = TRUE, filename = outFile, overwrite = TRUE)
      writeSemaphore(outFile)
      message(message_info("Summary cloud raster successfully calculated."))
      
      # Copy summarized file to cloud, deleting original
      gsCopy(outFile, outFileBkt, copySem = TRUE, deleteFrom = TRUE)
      
    }  
    
  }, logOutLevel = logOutLevel)
  
}

