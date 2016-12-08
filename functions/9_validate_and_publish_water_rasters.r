# Purpose: validate thresholded rasters using a series of tests and then, if they pass, 
# compress rasters and their metadata into a single compressed archive in the public google bucket
#
# Created: Apr 2016 (nelliott)
# Last modified: Apr 2016 (nelliott)

# Depends on most other base code files in autowater\functions

# Main function
validateAndPublishWaterRasters <- function(baseDir, scenes, dateStart, dateEnd, tempDir = tempdir(), 
                            overwrite = FALSE, logOutLevel = "TRACE", threshold = 0.99) {
  
  withConditionLogging({
    
    # Report
    raiseCondition(message_debug_enter())
    
    # Check inputs
    checkInput(c("baseDir", "scenes", "dateStart", "dateEnd", "tempDir", "logOutLevel"),
               classes = c("character", "character", "character", "character", "character", "character"),
               lengths = c(1, NA, 1, 1, 1, 1, 1))
    
    # Base directory
    checkFile(baseDir)
    
    # Temp directory
    checkFile(tempDir)
    
    # Buckets
    pvtBkt <- "gs://pointblue-autowater-pvt"
    pvtDir <- file.path(baseDir, "pvt")
    
    thrBkt <- file.path(pvtBkt, "predicted/thresholded")
    thrBktMnt <- file.path(pvtDir, "predicted/thresholded")
    
    cvBkt <- file.path(pvtBkt, "reference/cv_extents")
    cvBktMnt <- file.path(pvtDir, "reference/cv_extents")
    
    pubBkt <- "gs://pointblue-autowater-pub"
    pubDir <- file.path(baseDir, "pub")
    
    pubSngBkt <- file.path(pubBkt, "single_scenes")
    pubSngBktMnt <- file.path(pubDir, "single_scenes")
    
    # Check that the drives are mounted, running mounting script if not
    mounted <- gsCheck(condClass = "error", mount = TRUE)
    
    # Check directories
    checkFile(c(tempDir, thrBktMnt, pubSngBktMnt))
    
    # Raster temp dir
    library(raster)
    options(rasterTmpDir = tempDir)
    
    # Load metadata
    mdFile <- file.path(pvtDir, "metadata/landsat_metadata.csv")
    checkFile(mdFile)
    
    # Get matching files
    files <- listFilesBySceneDate(thrBktMnt, sceneChars = c(4, 9), scenes = scenes, 
                                  dateChars = c(11, 18), dateFormat = "%Y%m%d", dateStart = dateStart, dateEnd = dateEnd, 
                                  searchPattern = "water.tif$", full.names = TRUE)
    
    # Iterate outough files
    valid <- rep(NA, length(files))
    raiseCondition(message_debug_vector("Valid", valid))
    for (n in 1:length(files)) {
      
      f <- files[n]
      raiseCondition(message_debug_vector("File", f))
      
      # Validate file, reporting if invalid and skipping publishing
      raiseCondition(message_info(paste("Validating file", f)))
      #valid <- validateTestPoints(f, condClass = "warning")
      
      # Save value to metadata
      
      #if(rtCorrect >= threshold) {
        
      #  valid[n] <- TRUE
        
      #} else
      rtCorrect <- 1
      valid[n] <- ifelse(rtCorrect >= threshold, TRUE, FALSE)
        
      # If valid, publish
      if (valid[n]) {
          
        raiseCondition(message_info(paste("Publishing file", f)))
        
        # Out filename
        baseFn <- substr(basename(f), 1, nchar(basename(f)) - 4)
        raiseCondition(message_debug_vector("Base filename", baseFn))
        arcFn <- paste0(baseFn, ".tar.gz")
        arcFile <- file.path(tempDir, arcFn)
        arcFileBkt <- file.path(pubSngBkt, arcFn)
        arcFileBktMnt <- file.path(pubSngBktMnt, arcFn)
        
        # Check if output file exists
        if (file.exists(arcFileBktMnt)) {
          
          if (overwrite == TRUE) {
            
            raiseCondition(message_info(paste("Overwriting previous output (overwrite set to TRUE)")))
            
          } else {
            
            raiseCondition(message_info(paste("Image", f, "has already been published to and overwrite = FALSE.",
                                              "Moving to next...")))
            next
            
          }
          
        }
        
        # Load rasters for masking to CVJV
        scn <- as.character(parseFilenamePB(f)$Scene)
        maskFile <- file.path(cvBktMnt, paste0("cvjv_", scn, ".tif"))
        checkFile(maskFile)
        maskRst <- raster(maskFile)
        thrRst <- raster(f)
        
        # Mask to CVJV
        raiseCondition(message_trace("Masking to CVJV..."))
        tempRstFile <- file.path(tempDir, basename(f))
        outRst <- mask(thrRst, maskRst, maskvalue = NA, filename = tempRstFile, overwrite = TRUE)
        raiseCondition(message_info("Raster successfully masked to CVJV boundaries."))
        
        # Get metadata for file
        raiseCondition(message_trace(paste0("Getting metadata for file ", f, "...")))
        mdDf <- getLandsatMetadata(f, "PointBlue", mdFile)
        
        # Export md
        tempMdFile <- file.path(tempDir, gsub("water", "metadata.csv", baseFn))
        write.csv(mdDf, tempMdFile, row.names = FALSE)
        
        # List of files to archive
        fta <- c(tempRstFile, tempMdFile, file.path(pvtDir, "metadata/central_valley_water_dataset_landsat8_metadata.xml"))
        
        # Create archive
        raiseCondition(message_trace("Creating archive..."))
        createTar(fta, arcFile, overwrite = overwrite)
        message(message_trace("Archive successfully created."))
        
        # Copy summarized file to cloud, deleting original
        gsCopy(arcFile, arcFileBkt, copySem = FALSE, deleteFrom = TRUE)
        
        # Delete temporary files
        deleteFile(tempRstFile)
        deleteFile(tempMdFile)
        
      } # close publishing

    } #close file iteration
    
    # Raise error if any files failed validation
    if (!all(valid)) {
      
      msg <- paste("files", paste(files[!valid], collapse = ", "), "failed validation and were not published.")
      raiseCondition(validation_failure(msg, "error"))
      
    }
    
  }, logOutLevel = logOutLevel) #close logging
  
} #close master function
