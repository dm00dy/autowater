# Functions for validating landsat data
#
# nelliott, Aug 2016

# Requires packages rgdal and raster
# Requires conditions_base, input_testing, and file_manipulation
#
# Todo: define landsat metadata class

################
## Conditions ##
################

# Constructor function for a condition of (sub)class validation_failure.  Signalled when a validation test
# is failed
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
#   A condition object of classes name_of_function and condType.
validation_failure <- function(text, condClass = "error", trigger = NA, ...) {
  
  msgBase <- "Specified metadata file cannot be loaded and/or parsed"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}

# Constructor function for a condition of (sub)class projection mismatch.  Signalled when a two spatial
# objects that are supposed to share the same projection (crs) do not

# Returns:
#   A condition object of classes name_of_function and condType.
projection_mismatch <- function(text, condClass = "error", trigger = NA, ...) {
  
  msgBase <- "Specified spatial objects are not in the same projection (crs)"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}



###############
## Functions ##
###############

# Function for evaluating how well a classification performed against a known set of water/not-water points
#
# Args:
#   waterFile: The full filename of a binary water/not-water raster.
#   pointsFile: The full filename of the test points file. Must be an ESRI point shapefile with a column named
#               'WaterY_N' filled with binary values.  Projection should match that of the raster.
#   threshold (optional): The minimum required accuracy rate; defaults to 0.99
#   condClass (optional): The type of condition to raise when a file fails validation; defaults to "error"
#
# Returns:
#   A boolean value indicating whether or not the file passed validation.
validateTestPoints <- function(waterFile, pointsFile, threshold = 0.99, condClass = "error") {
  
  # Report
  message(message_debug_enter())
  
  # Check input
  checkInput(argNames = c("waterFile", "pointsFile", "threshold", "condClass"), 
             classes = c("character", "character", "numeric", "character"), 
             lengths = c(1, 1, 1, 1)
  )
  
  checkFile(c(waterFile, pointsFile))
  
  # Load packages
  library(rgdal)
  library(raster)
  
  # Load raster
  waterRst <- raster(waterFile)
  
  # Load points
  pfn <- substr(basename(pointsFile), 1, nchar(basename(pointsFile)) - 4)
  raiseCondition(message_debug_vector("Points shapefile basename without extension:", pfn))
  pointsShp <- readOGR(dsn = dirname(pointsFile), layer = pfn)
  
  # Check projection
  if (crs(waterRst) != crs(pointsShp)) {
    raiseCondition(projection_mismatch("validation raster and test points file have different projections.",
                                       "error"))
  }
  
  # Check if point shapefile
  
  # Check if has right column name
  
  # Extract
  pointsShp$WaterPrediction <- extract(waterRst, pointsShp)
  
  # Get number of points classified correctly
  
  # Report and return
  message(message_debug_exit())
  return(valid)
  
}
