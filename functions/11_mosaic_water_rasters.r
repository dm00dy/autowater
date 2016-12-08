# Purpose: compress rasters and their metadata into a single zip archive for publishing
#
# Created: Apr 2016 (nelliott)
# Last modified: Apr 2016 (nelliott)

# Depends on most other code files in autowater\functions

# Main function for stacking the rasters
mosaicWaterRasters <- function(baseDir, scenes, dateStart, dateEnd, tempDir = tempdir(), 
                            overwrite = FALSE, logOutLevel = "TRACE") {
  
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
    
    blkBkt <- file.path(pvtBkt, "reference/blanks")
    blkBktMnt <- file.path(pvtDir, "reference/blanks")
    
    thrBkt <- file.path(pvtBkt, "predicted/thresholded")
    thrBktMnt <- file.path(pvtDir, "predicted/thresholded")
    
    prjBkt <- file.path(thrBkt, "reprojected")
    prjBktMnt <- file.path(thrBktMnt, "reprojected")
    
    mscBkt <- file.path(pvtBkt, "mosaicked")
    mscBktMnt <- file.path(pvtDir, "mosaicked")
    
    # Check that the drives are mounted, running mounting script if not
    mounted <- gsCheck(condClass = "error", mount = TRUE)
    
    # Check directories
    checkFile(c(tempDir, thrBktMnt, prjBktMnt, mscBktMnt))
    
    # Packages
    library(rgdal)
    library(raster)
    
    # Raster temp dir
    options(rasterTmpDir = tempDir)
    
    # Break date range into 16-day chunks starting from 2013-04-16 and 2013-04-25
    mosaicDateStartA <- seq(as.Date("2013-04-16"), as.Date(dateEnd), 16)
    mosaicDateEndA <- mosaicDateStartA + 9
    
    mosaicDateStartB <- seq(as.Date("2013-04-25"), as.Date(dateEnd), 16)
    mosaicDateEndB <- mosaicDateStartB + 8
    
    mosaicDateDf <- data.frame("Cycle" = c(1:length(mosaicDateStartA), 1:length(mosaicDateStartB)), 
                              "DateStart" = c(mosaicDateStartA, mosaicDateStartB), 
                              "DateEnd" = c(mosaicDateEndA, mosaicDateEndB))
    
    # Remove dates before dateStart
    mosaicDateDf <- mosaicDateDf[mosaicDateDf$DateStart >= dateStart, ]
    
    # Loop across mosaic date ranges
    for (md in 1:nrow(mosaicDateDf)) {
      
      ds <- mosaicDateDf$DateStart[md]
      de <- mosaicDateDf$DateEnd[md]
      raiseCondition(message_info(paste("Working on mosaic for dates", ds, "to", de)))
      
      # Out filename
      mscFn <- paste0("L8_valley_", format(ds, format = "%Y%m%d"), "to", format(de, format = "%Y%m%d"), "_water.tif")
      mscFile <- file.path(tempDir, mscFn)
      mscFileBkt <- file.path(mscBkt, mscFn)
      mscFileBktMnt <- file.path(mscBktMnt, mscFn)
      
      # Check if output file exists
      if (file.exists(mscFileBktMnt)) {
        
        if (overwrite == TRUE) {
          
          raiseCondition(message_info(paste("Overwriting previous output (overwrite set to TRUE)")))
          
        } else {
          
          raiseCondition(message_info(paste("Mosaic for dates", ds, "to", de, "has already been created and overwrite = FALSE.",
                                            "Moving to next...")))
          next
          
        }
        
      }
      
      # Iterate through scenes, getting file if it exists and creating blank if missing
      raiseCondition(message_trace("Assembling scenes for mosaic"))
      rstList <- list()
      for (n in 1:length(scenes)) {
        
        # Get matching files
        scn <- scenes[n]
        files <- listFilesBySceneDate(thrBktMnt, sceneChars = c(4, 9), scenes = scn, 
                                      dateChars = c(11, 18), dateFormat = "%Y%m%d", 
                                      dateStart = as.character(ds), dateEnd = as.character(de), 
                                      searchPattern = "water.tif$", full.names = TRUE)
        
        # Load if single match
        if (length(files) == 1) {
          
          raiseCondition(message_trace(paste("Loading file for scene", scn)))
          rst <- raster(files)
          
          # Re-project if needed
          if (scn == "p42r35") {
            
            prjFile <- file.path(tempDir, basename(files))
            prjFileBkt <- file.path(prjBkt, basename(files))
            prjFileBktMnt <- file.path(prjBktMnt, basename(files))
            
            # Re-project if projected file does not exist
            if (!file.exists(prjFileBktMnt) | overwrite == TRUE) {
              
              raiseCondition(message_info(paste("Re-projecting scene", scn, "to match specified crs.")))
              
              outCRS <- "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
              prjRst <- projectRaster(from = rst, crs = outCRS, method = "ngb", filename = prjFile)
              raiseCondition(message_info(paste("File", basename(prjFile), "successfully re-projected.")))
              
              # Write semaphore
              writeSemaphore(prjFile)
              
              # Copy file to cloud, deleting original
              gsCopy(prjFile, prjFileBkt, copySem = TRUE, deleteFrom = TRUE)
              
            }
            
            # Load re-projected file
            raiseCondition(message_trace(paste("Using reprojected version of scene", scn)))
            rst <- raster(prjFileBktMnt)
          
          }
          
        
        # Load blank if none
        } else if (length(files) == 0) {
          
          # Set blank filename, using reprojected file if needed
          if (scn == "p42r35") {
            blkFile <- file.path(blkBktMnt, paste0(scn, "_blank_utm10.tif"))
          } else {
            blkFile <- file.path(blkBktMnt, paste0(scn, "_blank.tif"))
          }
          
          raiseCondition(message_trace(paste("Loading blank file for scene", scn)))
          rst <- raster(blkFile)
          
        # Else warn
        } else {
        
          stop("Too many files returned!")
      
        }
        
        # Assign 
        rstList[[n]] <- rst
      
      }
      
      # Mosaic
      mscRst <- createMosaic(rstList, mscFile, matchCRS = TRUE, tempDir = tempDir)
      #writeSemaphore(mscFile) #handled by createMosaic
      
      # Copy file to cloud, deleting original
      gsCopy(mscFile, mscFileBkt, copySem = TRUE, deleteFrom = TRUE)
      
    } #close mosaic date iteration
      
  }, logOutLevel = logOutLevel) #close logging
  
} #close master function
