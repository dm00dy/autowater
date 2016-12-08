# Purpose: prepare a set of landsat scenes for normalization (crop, resample, and change zeroes to no data)
#
# Created: 9-20-2012 (nelliott)
# Last modified: Apr 2014 (nelliott)

# Constructor function for a condition of (sub)class raster_snapping_failure.  Signalled when a raster cannot be
# snaped in extent and resolution (via crop and resample) correctly.
#
# Args:
#   text: Explanatory text to append to the condition message.
#   condType (optional): the overall class (type) of condition this is.  Must be "error", "warning", or "message".
#     Defaults to "error".
#   trigger (optional): The condition that caused this condition to be signalled.  Appended to returned condition.
#     Defaults to NA.
#   ... (optional): Additional arguments to append to the constructed condition, providing more detail.
#
# Returns:
#   A condition object of classes name_of_function and condType. Signalled when a specified file is missing.
raster_snapping_failure <- function(text, condClass = "error", trigger = NA, ...) {
  
  msgBase <- "Could not snap raster to specified extent and resolution"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}

# Constructor function for a condition of (sub)class raster_cloudmask_failure.  Signalled when a cloudmask cannot
# be successfully processed.
raster_cloudmask_failure <- function(text, condClass = "error", trigger = NA, ...) {
  
  msgBase <- "Could not process specified cloudmask(s)"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}

# Constructor function for a condition of (sub)class raster_ndi_failure.  Signalled when a normalized difference index
# cannot be successfully calculated.
raster_ndi_failure <- function(text, condClass = "error", trigger = NA, ...) {
  
  msgBase <- "Could not calculate specified normalized difference index"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}

# Constructor function for a condition of (sub)class raster_mosaic_failure.  Signalled when a mosaic
# cannot be successfully created.
raster_mosaic_failure <- function(text, condClass = "error", trigger = NA, ...) {
  
  msgBase <- "Could not create mosaic"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}

# Constructor function for a condition of (sub)class projection_mismatch.  Signalled when spatial projections
# (crs) do not match.
spatial_projection_mismatch <- function(text, condClass = "error", trigger = NA, ...) {
  
  msgBase <- "Spatial projections (crs) do not match"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}

# Function to set the raster tempdir to a specified directory using options(rasterTmpDir)
#
# Args:
#   tempDir: The directory to use as the raster tempdir
#   condClass (optional): The class of condition to raise if an error is encountered; defaults to warning.
#
# Returns:
#   nothing
setRasterTempDir <- function(tempDir, condClass = "warning") {
  
  # Report
  message(message_debug_enter())
  
  # Check input
  checkInput(c("tempDir", "condClass"), 
             classes = c("character", "character"), 
             lengths = c(1, 1),
             fxnName = "setRasterTempDir")
  
  # Temp dir
  if (file.exists(tempDir)) {
    raiseCondition(message_trace(paste0("Setting raster temp dir to ", tempDir)))
    options(rasterTmpDir = tempDir)
  } else {
    raiseCondition(file_missing(paste0("directory ", tempDir, " is invalid. Will continue using default tempdir;
                                           note that this may cause overflow issues."), condClass, file = tempDir))
  }
  
  # Report
  message(message_debug_exit())
  
}


# Function to crop and resample a raster to snap another
# Not vectorized.
#
# Args:
#   inFile: The raster file to crop and resample to snap a target.  Only accepts a single argument.
#   snapFile: The raster file of which to snap the extent and resolution. Single arg.
#   outFile: The filename to write to.
#   overwrite (optional): Whether or not to overwrite an existing file.  Defaults to FALSE.
#   ncores (optional): The number of cores to use. Defaults to one.
#   tempDir (optional): The directory in which to write temp files (including the intermediate crop file).
#     Defaults to what the option rasterTmpDir is set to.
#
# Returns:
#   The file of the cropped and resampled raster.
snapRaster <- function(inFile, snapFile, outFile, overwrite = FALSE, ncores = 1, 
                       tempDir = getOption("rasterTmpDir"), condClass = "error") {
  
  # Report
  message(message_debug_enter())
  
    # Check input
    checkInput(c("inFile", "snapFile", "outFile", "overwrite", "ncores", "tempDir"), 
                 classes = c("character", "character", "character", "logical", "numeric", "character"), 
                 lengths = c(1, 1, 1, 1, 1, 1),
                 fxnName = "snapRaster")
    
    # Check file existence
    checkFile(c(inFile, snapFile, tempDir))
    
    # Round ncores
    ncores <- round(ncores)
    
    # Check outFile existence
    if (file.exists(outFile)) {
      
      if (overwrite == TRUE) {
        message(message_info(paste("File", outFile, "already exists. Will overwrite (overwrite = TRUE).")))
      } else {
        message(message_info(paste("File", outFile, "already exists and overwrite = FALSE.  Skipping.")))
        return(outFile)
      }
      
    }
    
    # Load packages
    library(rgdal)
    library(raster)
    if (ncores > 1) library(snow)
    
    # Temp dir
    setRasterTempDir(tempDir)
  
    # Load
    inRst <- raster(inFile)
    snapRst <- raster(snapFile)
    
    # Check extent
    #if (extent())
    
    # Set intermediate output (cropFile); must ensure extension is lower case
    cropFile <- file.path(tempDir, basename(inFile))
    cropFile <- sub("([A-Z]*$)", "\\L\\1", cropFile, perl = TRUE)
    message(message_debug(paste("CropFile:", cropFile)))
    
    # Crop
    message(message_trace("Cropping to extent..."))
    cropRst <- crop(inRst, snapRst, filename = cropFile, overwrite = TRUE)
    
    # Write if it didn't
    if (!file.exists(cropFile) & exists("cropRst")) {
      raiseCondition(file_missing("Cropping complete but temp file did not write as expected. Attempting to re-write...", 
                                  condClass = "warning"))
      writeRaster(cropRst, cropFile, overwrite = TRUE)
    } 
    
    # Semaphore
    writeSemaphore(cropFile)
    
    # Resample
    message(message_trace("Resampling..."))
    if (ncores > 1) {
      message(message_trace(paste("Using", ncores, "cores.")))
      tryCatch({
                beginCluster(n = ncores)
                clusterR(cropRst, resample, args = list(y = snapRst, method = "ngb"), 
                         filename = outFile, overwrite = TRUE)
              },
                finally = endCluster()
              )
      
    } else {
      message(message_trace("Using one core."))
      resample(cropRst, snapRst, method = "ngb", filename = outFile, overwrite = TRUE)
    }
    
    # Semaphore
    writeSemaphore(outFile)
    
    # Delete crop file
    if (file.exists(outFile)) {
      deleteFile(cropFile)
    } else {
      raiseCondition(file_missing(outFile, "error", file = inFile))
    }
    
  # Report and return
  message(message_debug_exit())
  return(outFile)
  
}

# Function to create a cloud mask from the Landsat CF mask band
cloudMaskFromCF <- function(inFile, outFile, overwrite = FALSE, ncores = 1, tempDir = getOption("rasterTmpDir"), 
                            condClass = "error") {
  
  # Report
  message(message_debug_enter())
  
  tryCatch({
    
    # Check input
    checkInput(c("inFile", "outFile", "overwrite", "ncores", "tempDir"), 
               classes = c("character", "character", "logical", "numeric", "character"), 
               lengths = c(1, 1, 1, 1, 1))
    
    # Check file existence
    checkFile(c(inFile, tempDir))
    
    # Round ncores
    ncores <- round(ncores)
    
    # Check outFile existence
    if (file.exists(outFile)) {
      
      if (overwrite == TRUE) {
        message(message_info(paste("File", outFile, "already exists. Will overwrite (overwrite = TRUE).")))
      } else {
        message(message_info(paste("File", outFile, "already exists and overwrite = FALSE.  Skipping.")))
        return(outFile)
      }
      
    }
    
    # Load packages
    library(rgdal)
    library(raster)
    if (ncores > 1) library(snow)
    
    # Temp dir
    setRasterTempDir(tempDir)
    
    # Load
    inRst <- raster(inFile)
    
    # Process
    message(message_trace("Creating binary cloud mask from CFMask..."))
    #rclMat <- matrix(c() ncol = 2, byrow = TRUE)
    #rclCont <- matrix(c(-Inf, 3, 0, 4, 4, 1, 5, Inf, NA), ncol = 3, byrow = TRUE)
    rclMat <- matrix(c(1, 0, 2, 0, 3, 0, 4, 1, 255, NA), ncol = 2, byrow = TRUE)
    if (ncores > 1) {
      message(message_trace(paste("Using", ncores, "cores.")))
      tryCatch({
        beginCluster(n = ncores)
        clusterR(inRst, reclassify, args = list(rcl = rclMat), filename = outFile, overwrite = TRUE)
      },
      finally = endCluster()
      )
      
    } else {
      message(message_trace("Using one core."))
      reclassify(inRst, rcl = rclMat, filename = outFile, overwrite = TRUE)
    }
    
    # Semaphore
    writeSemaphore(outFile)
    
    }, error = function(e) {
      
      #e <- errorToWarning(e, TRUE)
      msg <- paste0(inFile, ".")
      raiseCondition(raster_cloudmask_failure(msg, "error", e, file = inFile))
      
    })
    
  
  # Report and return
  message(message_debug_exit())
  return(outFile)
  
}

# Function to calculate normalized differences (e.g., NDVI).  These take the format of:
# (Band x - Band y) / (Band x + Band y)
calcNormDiff <- function(file1, file2, outFile, overwrite = FALSE, 
                      tempDir = getOption("rasterTmpDir"), condClass = "error", ...) {

  # Report
  message(message_debug_enter())
  
  tryCatch({
    
    # Check input
    checkInput(c("file1", "file2", "outFile", "overwrite", "tempDir"), 
               classes = c("character", "character", "character", "logical", "character"), 
               lengths = c(1, 1, 1, 1, 1))
    
    # Check file existence
    checkFile(c(file1, file2, tempDir))
    
    # Check outFile existence
    if (file.exists(outFile)) {
      
      if (overwrite == TRUE) {
        message(message_info(paste("File", outFile, "already exists. Will overwrite (overwrite = TRUE).")))
      } else {
        message(message_info(paste("File", outFile, "already exists and overwrite = FALSE.  Skipping.")))
        return(outFile)
      }
      
    }
    
    # Load packages
    library(rgdal)
    library(raster)

    # Temp dir
    setRasterTempDir(tempDir)
    
    # Load
    r1 <- raster(file1)
    r2 <- raster(file2)
    
    # Process
    message(message_trace("Creating normalized difference index using one core..."))
    ndi <- function(r1, r2) {
      rr1 <- r1[]
      rr2 <- r2[]
      r1[] <- (rr1 - rr2) / (rr1 + rr2)
      return(r1)
    }
    rOut <- ndi(r1, r2)
    message(message_debug("Exporting raster..."))
    writeRaster(rOut, filename = outFile, overwrite = TRUE, ...)
    
    # Semaphore
    writeSemaphore(outFile)
    
  }, error = function(e) {
    
    #e <- errorToWarning(e, TRUE)
    msg <- paste0("with files ", file1, ", ", file2, ".")
    raiseCondition(raster_ndi_failure(msg, condClass, e, file = file1))
    
  })
  
  # Report and return
  message(message_debug_exit())
  return(rOut)

}


# Function to create a mosaic from a list of rasters or raster files
createMosaic <- function(x, outFile, outCRS = "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", 
                         matchCRS = FALSE, overwrite = FALSE, fun = "mean", na.rm = TRUE,
                         tempDir = getOption("rasterTmpDir"), condClass = "error") {
  
  # Report
  message(message_debug_enter())
  
  tryCatch({
    
    # Check input
    checkInput(c("x", "outFile", "outCRS", "matchCRS", "overwrite", "tempDir"), 
               classes = c(NA, "character", "character", "logical", "logical", "character"), 
               lengths = c(NA, 1, 1, 1, 1, 1))
    
    # Check there are multiple rasters to mosaic
    if (length(x) < 2) {
      
      raiseCondition(invalid_value("createMosaic requires at least two rasters", "error", x))
    
    }
    
    # Load packages
    library(rgdal)
    library(raster)
    
    # Check x is a list of files or rasters
    if (all(sapply(x, class) == "RasterLayer")) {
      
      raiseCondition(message_debug("Passed list of rasters"))
      rns <- sapply(x, names)
      checkFile(tempDir)
      
    # If files, load
    } else if (all(sapply(x, is.character))) {
      
      raiseCondition(message_debug("Passed vector or list of raster files"))
      rns <- basename(unlist(x))
      
      checkFile(c(unlist(x), tempDir))
      x <- sapply(x, raster)
      
    } else {
      
      raiseCondition(invalid_class("List x passed to create mosaic must be a list of rasters or a list of raster files."))
      
    }
    
    # Check projection
    if (!compareRaster(x, extent = FALSE, rowcol = FALSE, crs = TRUE, stopiffalse = FALSE)) {
      
      if (matchCRS == TRUE) {
        
        raiseCondition(spatial_projection_mismatch(paste("rasters passed to mosaicRaster have different coordinate systems.", 
                                                 "Will attempt to re-project all to match specified CRS."), "warning"))
        
        # Loop across rasters
        for (n in 1:length(x)) {
          
          if (identical(outCRS, projection(x[[n]]))) {
            
            raiseCondition(message_debug(paste("Raster", rns[n], "matches specified projection outCRS", outCRS, ".")))
            
          } else {
            
            raiseCondition(message_trace(paste("Re-projecting", rns[n], "to", outCRS)))
            x[[n]] <- projectRaster(from = x[[n]], crs = outCRS, method = "ngb", filename = file.path(tempDir, rns[n]))
            
          }
          
        }
        
      } else {
        
        raiseCondition(spatial_projection_mismatch("cannot mosaic rasters of different coordinate systems.", "error"))
        
      }
       
    }
    
    # Check outFile existence
    if (file.exists(outFile)) {
      
      if (overwrite == TRUE) {
        message(message_info(paste("File", outFile, "already exists. Will overwrite (overwrite = TRUE).")))
      } else {
        message(message_info(paste("File", outFile, "already exists and overwrite = FALSE.  Skipping.")))
        return(raster(outFile))
      }
      
    }
    
    # Temp dir
    setRasterTempDir(tempDir)
    
    # Build list for call
    callList <- x
    names(callList)[1] <- "x"
    names(callList)[2] <- "y"
    callList[["fun"]] <- fun
    callList[["na.rm"]] <- na.rm
    callList[["tolerance"]] <- 0.5
    callList[["filename"]] <- outFile
    callList[["overwrite"]] <- TRUE
    
    # Create mosaic
    raiseCondition(message_trace("Creating mosaic of passed files..."))
    msc <- do.call(mosaic, callList)
    message(message_info("Rasters successfully mosaiced."))
    
    # Semaphore
    writeSemaphore(outFile)

  }, error = function(e) {
    
    if (exists("rns")) {
      
      msg <- paste0("from rasters ", paste(rns, collapse = ", "), ".")
      
    } else {
      
      msg <- paste0("from rasters passed to createMosaic.")
      
    }
    
    raiseCondition(raster_mosaic_failure(msg, condClass, e, rasters = x))
    
  })
  
  # Report and return
  message(message_debug_exit())
  return(msc)
  
}

validateTestPoints <- function() {
  
  
}
