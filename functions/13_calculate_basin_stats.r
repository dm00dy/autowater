# Purpose: validate thresholded rasters using a series of tests and then, if they pass, 
# compress rasters and their metadata into a single compressed archive in the public google bucket
#
# Created: Apr 2016 (nelliott)
# Last modified: Apr 2016 (nelliott)

# Depends on most other base code files in autowater\functions

# Main function
calculateBasinStats <- function(baseDir, scenes, dateStart, dateEnd, tempDir = tempdir(), 
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
    
    cvBkt <- file.path(pvtBkt, "reference/cv_extents")
    cvBktMnt <- file.path(pvtDir, "reference/cv_extents")
    
    mscBkt <- file.path(pvtBkt, "mosaicked")
    mscBktMnt <- file.path(pvtDir, "mosaicked")
    
    covBkt <- file.path(mscBkt, "coverage")
    covBktMnt <- file.path(mscBktMnt, "coverage")
      
    statBkt <- file.path(pvtBkt, "stats")
    statBktMnt <- file.path(pvtDir, "stats")
    
    pubStatBkt <- "gs://pointblue-autowater-pub/stats"
    
    # Check that the drives are mounted, running mounting script if not
    mounted <- gsCheck(condClass = "error", mount = TRUE)
    
    # Check directories
    checkFile(c(tempDir, mscBktMnt, covBktMnt, statBktMnt))
    
    # Raster temp dir
    library(raster)
    options(rasterTmpDir = tempDir)

    # Load basins file
    bsnRst <- raster(file.path(cvBktMnt, "cvjv_valley_basins.tif"))
    
    # Basin areas
    bsnDf <- data.frame(ID = 1:9, Name = c("American", "Butte", "Colusa", "Delta", "San Joaquin", "Suisun", "Sutter", "Tulare", "Yolo"),
                           AreaSqmFull = c(2101723850.64, 2459358201.84, 4647736657.25, 6824370797.77, 11516577322.30, 
                                           615678339.09, 960432757.21, 26972755151.00, 2055884853.95))
    
    # Get matching files
    files <- listFilesBySceneDate(mscBktMnt, sceneChars = c(4, 9), scenes = "valley", 
                                  dateChars = c(11, 18), dateFormat = "%Y%m%d", dateStart = dateStart, dateEnd = dateEnd, 
                                  searchPattern = "water.tif$", altSceneValue = "valley", full.names = TRUE)
    
    # Iterate outough files
    for (f in files) {
      
      raiseCondition(message_info(paste("Calculating stats for file", f)))
      
      # Out filename
      baseFn <- substr(basename(f), 1, nchar(basename(f)) - 4)
      raiseCondition(message_debug_vector("Base filename", baseFn))
      statFn <- paste0(baseFn, "_basin.csv")
      statFile <- file.path(tempDir, statFn)
      statFileBkt <- file.path(statBkt, statFn)
      statFileBktMnt <- file.path(statBktMnt, statFn)
      
      # Check if output file exists
      if (file.exists(statFileBktMnt)) {
        
        if (overwrite == TRUE) {
          
          raiseCondition(message_info(paste("Overwriting previous output (overwrite set to TRUE)")))
          
        } else {
          
          raiseCondition(message_info(paste("Stats for mosaic", f, "have already beeen calculated and overwrite = FALSE.",
                                            "Moving to next...")))
          next
          
        }
        
      }
      
      # Load mosaiced file
      mscRst <- raster(f)
      
      # Get/create coverage raster
      covFn <- gsub("water", "coverage", basename(f))
      covFile <- file.path(tempDir, covFn)
      covFileBkt <- file.path(covBkt, covFn)
      covFileBktMnt <- file.path(covBktMnt, covFn)
      
      # Create coverage raster if missing
      if(!file.exists(covFileBktMnt) | overwrite == TRUE) {

        raiseCondition(message_info("Creating coverage raster"))
        covRst <- mscRst
        # set every non-NA pixel to one; then area covered is the sum (times pixel resolution)
        covRst <- reclassify(covRst, c(-Inf, Inf, 1), filename = covFile, overwrite = TRUE) 
        raiseCondition(message_trace("Coverage raster created"))
        
        # Write semaphore
        writeSemaphore(covFile)
        
        # Copy file to cloud, deleting original
        gsCopy(covFile, covFileBkt, copySem = TRUE, deleteFrom = TRUE)
      
      }
      
      # Load coverage ratser
      raiseCondition(message_trace("Loading coverage raster from file..."))
      covRst <- raster(covFileBktMnt)
      
      # Base values
      statDf <- NULL
      cover <- NULL
      water <- NULL
      pixelsObs <- NA
      pixelsWater <- NA
      areaObs <- NA
      areaWater <- NA
      pctObs <- NA
      pctWater <- NA
      estWater <- NA
      thresh <- 0.1552574
      
      # Zonal stats
      raiseCondition(message_trace("Calculating coverage by basin..."))
      cover <- zonal(covRst, bsnRst, fun = "sum", na.rm = TRUE)
      raiseCondition(message_debug_vector("Coverage pixels:", cover))
      
      raiseCondition(message_trace("Calculating water by basin..."))
      water <- zonal(mscRst, bsnRst, fun = "sum", na.rm = TRUE)
      raiseCondition(message_debug_vector("Water pixels:", water))

      # Append to df
      # Loop through zonal stats
      for (zsum in 1:nrow(cover)) {
        
        # Pull info from zonal stats
        basinId <- cover[zsum, 1]
        pixelsObs <- cover[zsum, 2]
        pixelsWater <- water[zsum, 2]
        
        # Skip if basinId is zero (not valid)
        if (basinId == 0) next
        
        # Link with basin name from bsnDf
        bsn <- bsnDf$Name[bsnDf$ID == basinId]
        bsnArea <- bsnDf$AreaSqm[bsnDf$ID == basinId]
        
        # If pixelsObs < 50
        if ((pixelsObs < 50) | (pixelsWater < 5)) {
          
          # Set values to 0 or NA
          areaObs <- 0
          areaWater <- NA
          pctObs <- NA
          pctWater <- NA
          estWater <- NA
          
        } else {
          
          # Multiply by cell resolution to get area coverage in sq meters (rather than by pixel)
          areaObs <- pixelsObs * 30 * 30
          areaWater <- pixelsWater * 30 * 30
          
          # Calculate percentages
          if (areaWater >= areaObs) { 
            
            raiseCondition(invalid_value("Percent flood calculated as >= 100!", "warning"))
            pctWater <- NA
            areaWater <- NA
            areaObs <- 0
            
          } else {
            
            # Extrapolate total flooded area
            pctObs <- (areaObs / bsnArea) * 100
            pctWater <- (areaWater / areaObs) * 100
            estWater <- (pctWater / 100) * bsnArea
            
          }
          
        }
        
        # Append to df
        tempDf <- data.frame(BasinName = as.character(bsn), Mosaic = basename(f), 
                             MosaicDateStart = as.character(parseDate(substr(basename(f), 11, 18), "%Y%m%d")$DateOut), 
                             MosaicDateEnd = as.character(parseDate(substr(basename(f), 21, 28), "%Y%m%d")$DateOut),
                             ObservedArea = areaObs, ObservedAreaWater = areaWater, PercentObserved = pctObs, 
                             PercentWater = pctWater, EstimatedAreaWater = estWater, Threshold = thresh)
        statDf <- rbind(statDf, tempDf)
        
      }
      
      # Export data (can write csvs directly to drive without separate copy step)
      write.csv(statDf, statFileBktMnt, row.names = FALSE)
      
      # Copy file to cloud, deleting original
      #gsCopy(statFile, statFileBkt, copySem = FALSE, deleteFrom = TRUE)
      
      # Append all files
      statFiles <- list.files(statBktMnt, pattern = "L8_valley.*water_basin.csv", full.names = TRUE)
      allDf <- do.call(rbind, lapply(statFiles, function(x) read.csv(x, stringsAsFactors = FALSE)))
      write.csv(allDf, file.path(statBktMnt, "basin_stats.csv"))
      
      # Copy to public bucket
      gsCopy(file.path(statBkt, "basin_stats.csv"), file.path(pubStatBkt, "basin_stats.csv"), copySem = FALSE, deleteFrom = FALSE)
      
    } #close file iteration
    
  }, logOutLevel = logOutLevel) #close logging
  
} #close master function
