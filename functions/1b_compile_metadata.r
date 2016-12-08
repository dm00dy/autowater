# Function to process metadata files in folder
#
#


# Depends on most other code files in autowater\functions

compileMetadata <- function(baseDir, scenes, dateStart, dateEnd, tempDir = tempdir(), logOutLevel = "TRACE", overwrite = FALSE) {
  
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
    mdBkt <- file.path(pvtBkt, "metadata")
    mdBktMnt <- file.path(baseDir, "pvt/metadata")
    mdRawBkt <- file.path(mdBkt, "raw")
    mdRawBktMnt <- file.path(baseDir, "pvt/metadata/raw")
    mdFlatBkt <- file.path(mdBkt, "flat")
     
    # Check that the drives are mounted, runing mounting script if not
    mounted <- gsCheck(condClass = "error", mount = TRUE)
    
    # Raster temp dir
    library(raster)
    options(rasterTmpDir = tempDir)
    
    # Load sourcing code
    #source(file.path(codeDir, "loading_code_independent.r"))
    
    # Source code files
    #sourceDir(codeDir)
    
    # Get matching metadata files
    files <- listFilesBySceneDate(mdRawBktMnt, sceneChars = c(4, 9), scenes = scenes, 
                                  dateChars = c(10, 16), dateFormat = "%Y%j", dateStart = dateStart, dateEnd = dateEnd, 
                                  searchPattern = "MTL.txt$", full.names = TRUE)
    
    # Loop across files
    for (f in files) {
      
      # Check file existence
      if (file.exists(paste0(f, ".compiled"))) {
        
        raiseCondition(message_info(paste("Metadata for", basename(f), "already compiled.")))
        
      # Else compile
      } else {
        
        raiseCondition(message_info(paste0("Processing metadata file ", basename(f), "...")))
        mdRawFn <- basename(f)
    
        # Process
        mdFileFlat <- file.path(tempDir, paste0(mdRawFn, ".csv"))
        mdFileAppend <- file.path(mdBktMnt, "landsat_metadata.csv")
        mdDf <- processMetadata(f, mdFileFlat = mdFileFlat, mdFileAppend = mdFileAppend, 
                                condClass = "Warning")
        
        # Copy to cloud
        gsCopy(mdFileFlat, file.path(mdFlatBkt, basename(mdFileFlat)), copySem = FALSE, deleteFrom = TRUE)
        #gsCopy(mdFileAppend, file.path(mdBkt, basename(mdFileAppend)), copySem = FALSE, deleteFrom = FALSE)
        
        # Write semaphore
        writeSemaphore(f, ".compiled")
        raiseCondition(message_info(paste("Metadata file", basename(f), "successfully copied.")))
        
      }
      
    } #close iteration over files
    
  }, logOutLevel = logOutLevel) #close condition logging
  
}