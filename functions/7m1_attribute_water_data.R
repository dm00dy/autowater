# Purpose: functions used to attribute central valley water data
#
# Created: Apr 2016 (nelliott)
# Last modified: Apr 2016 (nelliott)

# Depends on most other code files in autowater\functions


# Function for attributing training data
attributeWaterDataTraining <- function(baseDir, scenes, dateStart, dateEnd, tempDir = tempdir(), 
                                       overwrite = FALSE, logOutLevel = "TRACE", cloudCoverThreshold = 5) {
  
  # Can only do one scene at a time
  if (length(scenes) > 1) {
    raiseCondition(invalid_value("only a single scene may be passed to attributeWaterDataTraing at a time. Using first
                                 element of vector only.", "warning", invalidValue = scenes))
  }
  
  # Parse the scene name
  scenes <- parseScene(scenes)[[1]][1]
  
  # Attribute
  attributeWaterData(baseDir, scenes = scenes, dateStart, dateEnd, tempDir = tempDir, 
                       overwrite = overwrite, logOutLevel = logOutLevel, cloudCoverThreshold = cloudCoverThreshold, 
                       waterFiles = file.path(baseDir, paste0("pvt/water_data/initial_training/water_data_", scenes)),
                       waterFileDateColumn = "ObsDate", waterFileDateFormat = "%Y%m%d")
  
}

# Function for attributing validation data

# Main function for attributing water data
# Do not include shapefile extension in waterFiles
attributeWaterData <- function(baseDir, scenes, dateStart, dateEnd, tempDir = tempdir(), 
                            overwrite = FALSE, logOutLevel = "TRACE", cloudCoverThreshold = 5, waterFiles = NA,
                            waterFileDateColumn = "ObsDate", waterFileDateFormat = "%Y%m%d") {
  
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
    
    # Water files
    checkFile(paste0(waterFiles, ".shp"))
    
    # Buckets
    pvtBkt <- "gs://pointblue-autowater-pvt"
    pvtDir <- file.path(baseDir, "pvt")
    
    stkBktMnt <- file.path(pvtDir, "stacked")
    
    mskBktMnt <- file.path(stkBktMnt, "masked")
    
    attBkt <- file.path(pvtBkt, "water_data/attributed")
    attBktMnt <- file.path(pvtDir, "water_data/attributed")
    
    # Check that the drives are mounted, runing mounting script if not
    mounted <- gsCheck(condClass = "error", mount = TRUE)
    
    # Check directories
    checkFile(c(tempDir, stkBktMnt, mskBktMnt))
    
    # Raster temp dir
    library(rgdal)
    library(raster)
    options(rasterTmpDir = tempDir)
    
    # Bands
    bnds <- c(paste0("Band", c(1:7, 9:11)), "NDWI", "MNDWI")
    
    # Get matching files from metadata
    mdDf <- read.csv(file.path(pvtDir, "metadata/landsat_metadata.csv"))
    message(message_debug_vector("mdDf column names", names(mdDf)))
    
    # Iterate through scenes
    for (scn in scenes) {
      
      message(message_trace(paste("Working on images for scene", scn)))
      pth <- substr(scn, 2, 3)
      rw <- substr(scn, 5, 6)
      message(message_debug_vector("DATE_ACQUIRED:", mdDf$DATE_ACQUIRED))
      scnDf <- mdDf[mdDf$WRS_PATH == pth & mdDf$WRS_ROW == rw, ]
      
      # Loop across water files
      for (wf in waterFiles) {
        
        message(message_trace(paste("Working on water data from file", wf)))
        
        # Get unique dates
        wShp <- readOGR(dsn = dirname(wf), layer = basename(wf))
        uniqueDates <- unique(wShp[[waterFileDateColumn]])
        
        # Loop across unique dates
        for (ud in uniqueDates) {
          
          message(message_trace(paste("Working on data from date", ud)))
          uShp <- subset(wShp, wShp$ObsDate == ud)
          
          # Check file existence
          outFn <- paste0(basename(wf), "_", ud, ".csv")
          if (file.exists(file.path(attBktMnt, outFn)) & overwrite != TRUE) {
            
            raiseCondition(message_info(paste("Attributed file", outFn, "has already been created. Moving to next...")))
            next
          
          }
          
          # Calculate date difference
          message(message_debug_vector("DATE_ACQUIRED:", scnDf$DATE_ACQUIRED))
          scnDf$DaysOff <- abs(as.Date(scnDf$DATE_ACQUIRED) - as.Date(ud, format = waterFileDateFormat))
          message(message_debug_vector("DaysOff:", scnDf$DaysOff))
          
          # Get matching Landsat image from metadata
          mDf <- scnDf[scnDf$DaysOff <= 7, ]
          message(message_debug_vector("Landsat date:", mDf$DATE_ACQUIRED))
          message(message_debug_vector("DaysOff:", mDf$DaysOff))
          
          # Check matches
          if (nrow(mDf) == 0) {
            
            raiseCondition(invalid_value("No Landsat image from within 7 days of data. Moving to next...", "warning"))
            next
            
          } else if (nrow(mDf) > 1) {
            
            raiseCondition(invalid_value("Multiple images listed as available--not possible!  Check script.", "error"))
          
          # Otherwise load and attribute
          } else {
            
            # Check cloud cover, loading specified raster
            cloudCover <- mDf$CLOUD_COVER
            fnDf <- parseFilenameUSGS(as.character(mDf$LANDSAT_SCENE_ID))
            
            # Load unmasked image if below
            if (cloudCover < cloudCoverThreshold) {
              
              message(message_trace(paste0("Cloud cover of image (", cloudCover, 
                                           "%) is less than the minimum specified to use cloud mask.")))
              inFile <- file.path(stkBktMnt, createFilenamePB(fnDf, newType = "stacked", newVersion = "", newExt = ".tif"))
              
            # Otherwise, load masked image
            } else {
              
              message(message_trace(paste0("Cloud cover of image (", cloudCover, 
                                           "%) is more than the minimum specified to use cloud mask.")))
              inFile <- file.path(mskBktMnt, createFilenamePB(fnDf, newType = "masked", newVersion = "", newExt = ".tif"))
               
            }
            
            # Load
            message(message_trace(paste0("Loading image ", inFile, "...")))
            if (!checkFile(inFile, "warning")) {
              next
            }
            inStk <- stack(inFile)
            
            # Extract landsat data
            message(message_trace("Attributing..."))
            lsatMat <- extract(inStk, uShp, fun = mean, na.rm = TRUE)
            
            # Check output
            lsatDf <- data.frame(lsatMat)
            colnames(lsatDf) <- bnds
            if (nrow(lsatDf) < 1 | all(is.na(lsatDf$Band1))) {
              raiseCondition(invalid_value("No data extracted.  Skipping...", "warning"))
              next
            }
            
            # Append to data frame
            attDf <- cbind(uShp@data, lsatDf, LsatDate = rep(mDf$DATE_ACQUIRED), DaysOff = rep(mDf$DaysOff))
            attDf <- subset(attDf, !is.na(attDf$Band1))
            write.csv(attDf, file.path(tempDir, outFn))
            
            # Copy to google drive
            gsCopy(file.path(tempDir, outFn), file.path(attBkt, outFn), copySem = FALSE, deleteFrom = TRUE)
            message(message_trace("Values extracted and written. Moving to next shapefile..."))
            
            
          } #close load and attribute
          
        } #close looping across unique dates
        
      } #close looping across water files
      
    } #close looping across scene

  }, logOutLevel = logOutLevel) #close withConditionLogging
  
} #close function
