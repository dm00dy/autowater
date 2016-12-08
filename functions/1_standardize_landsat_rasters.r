# Purpose: prepare a set of landsat scenes for normalization (crop, resample, and change zeroes to no data)
#
# Created: 9-20-2012 (nelliott)
# Last modified: Apr 2014 (nelliott)

# Depends on most other code files in autowater\functions

standardizeLandsatRasters <- function(baseDir, scenes, dateStart, dateEnd, tempDir = tempdir(), logOutLevel = "TRACE",
                                      overwrite = FALSE,
                                      wantedBands = c(paste0("sr_band", 1:7, ".tif"), 
                                                     paste0("band", 9:11, ".tif"), "qa.tif", 
                                                      "cfmask.tif", "cfmask_conf.tif", "sr_cloud.tif")
) {
  
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
    cdrDir <- file.path(baseDir, "cdr")
    prcDir <- file.path(baseDir, "processing")
    extrDir <- file.path(prcDir, "extracted")
    mdDir <- file.path(prcDir, "metadata")
    refDir <- file.path(prcDir, "reference/scene_extents")
    cropDir <- file.path(prcDir, "cropped")
    snapDir <- file.path(prcDir, "snapped")
    
    # Buckets
    pvtBkt <- "gs://pointblue-autowater-pvt"
    mdBkt <- file.path(pvtBkt, "metadata")
    mdRawBkt <- file.path(mdBkt, "raw")
    mdRawBktMnt <- file.path(baseDir, "pvt/metadata/raw")
    mdFlatBkt <- file.path(mdBkt, "flat")
    snapBkt <- file.path(pvtBkt, "snapped")
    
    # Check that the drives are mounted, runing mounting script if not
    mounted <- gsCheck(condClass = "error", mount = TRUE)
    
    # Check directories
    checkFile(c(cdrDir, prcDir, extrDir, mdDir, refDir, cropDir, snapDir, mdRawBktMnt))
    
    # Raster temp dir
    library(raster)
    options(rasterTmpDir = tempDir)
    
    # Load sourcing code
    #source(file.path(codeDir, "loading_code_independent.r"))
    
    # Source code files
    #sourceDir(codeDir)

    # Get matching folders
    folders <- listFilesBySceneDate(cdrDir, sceneChars = c(4, 9), scenes = scenes, 
                                    dateChars = c(10, 16), dateFormat = "%Y%j", dateStart = dateStart, dateEnd = dateEnd, 
                                    searchPattern = "", full.names = TRUE)
  
    # Iterate through files
    for (fld in folders) {
    
      message(message_info(paste0("Processing (extracting and snapping) folder ", fld, "...")))
      
      # Get file to extract
      tarFile <- list.files(fld, pattern = ".tar.gz$", full.names = TRUE)
      
      # Warn and skip if missing
      if (length(tarFile) == 0) {
        
        raiseCondition(file_missing(paste(tarFile, ". File not extracted as expected. Skipping to next..."),
                                    "warning", file = tarFile))
        next
        
      }
      
      # Check if it has been extracted (look for semaphore file.extracted)
      if (file.exists(paste0(tarFile, ".extracted")) & overwrite != TRUE) {
        
        raiseCondition(message_info(paste("File", basename(tarFile), "already extracted; not set to re-extract (overwrite).")))
      
      # Else extract
      } else {
        
        # Extract
        message(message_info(paste0("Extracting file ", basename(tarFile), " to ", extrDir, ".")))
        exitCode <- extractTar(tarFile, extrDir)
        message(message_debug(paste("Tar extraction exit code:", exitCode)))
      
      }
      
      # List extracted files
      extrTifs <- list.files(extrDir, pattern = paste0(basename(fld), ".*(TIF|tif)$"), full.names = TRUE)
      message(message_debug_vector("Extracted tifs:", extrTifs))
      
      # If none, assume already extracted and go to next
      if (length(extrTifs) == 0) {
        raiseCondition(message_info("No extracted tifs left to process. Moving to next folder."))
        next
      }
      
      # Copy metadata file to cloud
      mdRawFn <- paste0(substr(basename(extrTifs[1]), 1, 22), "MTL.txt")
      mdFileIn <- file.path(extrDir, mdRawFn)
      if (file.exists(mdFileIn)) {
        gsCopy(mdFileIn, file.path(mdRawBkt, mdRawFn), copySem = FALSE, deleteFrom = FALSE)
        raiseCondition(message_info("Metadata file successfully copied."))
      } else {
        raiseCondition(file_missing(paste0("metadata file ", mdFileIn, "."), "warning", file = mdFileIn))
      }
 
      # Get matching snap file
      # problem with parseLandsatFilename
      fnDf <- parseFilenameUSGS(extrTifs[1])
      snapFile <- file.path(refDir, paste0(fnDf$Scene, "_crop_extent_small.tif"))
      
      # Error if missing snap file
      if (!file.exists(snapFile)) {
        
        raiseCondition(file_missing(paste0("snap file ", snapFile, ". Moving to next..."), "warning", 
                                    file = snapFile))
        next
        
      }
      
      # Delete unneeded files
      extrFiles <- list.files(extrDir, pattern = basename(fld), full.names = TRUE)
  
      wantedFiles <- extrFiles[unique(unlist(lapply(wantedBands, FUN = function(x) {grep(x, extrFiles)})))]
      message(message_debug_vector("Wanted files:", wantedFiles))
      
      unwantedFiles <- extrFiles[!(extrFiles %in% wantedFiles)]
      message(message_debug_vector("Unwanted files:", unwantedFiles))
      
      if (length(unwantedFiles) > 0) deleteFile(unwantedFiles, deleteSem = FALSE)
      
      # Loop across extracted files
      if (length(wantedBands) > 0) {
        
        for (wb in wantedBands) {
          
          # Build filename
          extrFile <- file.path(extrDir, paste0(substr(basename(extrTifs[1]), 1, 22), wb))
          
          # Convert filename
          fnDf <- parseFilenameUSGS(extrFile)
          #createFilenamePB <- function(fnDf, newType = NA, newVersion = NA, newExt = NA, 
            #dateFormatIn = "%Y-%m-%d", dateFormatOut = "%Y%m%d") {
          outFn <- createFilenamePB(fnDf, newType = "snapped", newVersion = "00")
          outFile <- file.path(snapDir, outFn)
          message(message_debug(paste("OutFn:", outFn)))
          outFileBkt <- file.path(snapBkt, outFn)
          outFileBktMnt <- file.path("/home/blue/pvt/snapped", outFn)
          
          # Check if output file exists
          if (file.exists(outFileBktMnt)) {
            
            raiseCondition(message_info(paste("File", basename(extrFile), "already snapped; moving to next.")))
            
            # Delete extracted file
            if (file.exists(extrFile)) deleteFile(extrFile, deleteSem = FALSE, condClass = "message")
            next
            
          # If not processed, check extracted file exists
          } else if (!file.exists(extrFile)) {
    
            raiseCondition(file_missing(paste0("could not find expected extracted file ", extrFile, 
                                              ". Attempting to re-extract..."), "warning", file = extrFile))
            
            # Extract
            message(message_info(paste0("Extracting file ", basename(tarFile), " to ", extrDir, ".")))
            exitCode <- extractTar(tarFile, extrDir)
            message(message_debug(paste("Tar extraction exit code:", exitCode)))
            
          }
          
          # Snap raster (crop and resample)
          #snapRaster <- function(inFile, snapFile, outFile, ncores = 1, tempDir = getOption("rasterTmpDir")) {
          message(message_trace(paste0("Snapping raster ", extrFile, "...")))
          tryCatch(snapRaster(extrFile, snapFile, outFile, overwrite = FALSE, ncores = 1, tempDir = cropDir),
                   
            error = function(e) {
             
             raiseCondition(raster_snapping_failure(paste0(". Will attempt to solve by re-extracting file ", 
                                                           extrFile, "."), "warning", e, file = extrFile))
              
             tryCatch({
                      
              exitCode <- extractTar(tarFile, extrDir)
              snapRaster(extrFile, snapFile, outFile, overwrite = FALSE, ncores = 1, tempDir = cropDir)
                      
              }, error = function(e) {
                
                raiseCondition(raster_snapping_failure(paste0(". Could not process ", extrFile, ". Aborting."), "error",
                                                       e, file = extrFile))
                
              })
             
            })
          message(message_info(paste("Raster", basename(extrFile), "successfully snapped.")))
          
          # Delete extracted file
          deleteFile(extrFile, deleteSem = FALSE)
          
          # Copy snapped file to cloud, deleting original
          gsCopy(outFile, outFileBkt, copySem = TRUE, deleteFrom = TRUE)
          
        } #close iteration across wantedBands
        
      } #close conditional checking wantedBands
      
    } #close iteration across folders
    
    # Report
    raiseCondition(message_debug_exit())
    
  }, logOutLevel = logOutLevel)
  
}

