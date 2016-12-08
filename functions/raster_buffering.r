# Testing a better buffer function
#
# nelliott, June 2015

# Simple version of buffer--within a certain number of cells
# Overbuffers: buffers as a square rather than circle, including cells on diagonal that are ok

bufferSimple <- function(x, width = 1) UseMethod("bufferSimple")

bufferSimple.default <- function(x, width = 1) {
  stop("Incompatible data type: must be numeric vector, matrix, or raster layer.")
}

# Numeric method for simple buffer
bufferSimple.numeric <- function(x, width = 1) {
  
  # Report
  #message(message_debug_enter())
  
  # Determine rows in the column that have the mask value (1)
  withMask <- which(x == 1)
  
  # Calculate the rows within a given number of cells of the mask values
  inRangeMin <- pmax(1, withMask - width)
  inRangeMax <- pmin(length(x), withMask + width)
  inRange <- Map(`:`, inRangeMin, inRangeMax)
  inRange <- unique(unlist(inRange, use.names = FALSE))
  
  # Replace values in range with the mask value
  x[inRange] <- 1
  return(x)
  
  # Report and return
  #message(message_debug_exit())
  return(x)
  
}

# Matrix method for simple buffer
bufferSimple.matrix <- function(m, width = 1) {
  
  # Report
  message(message_debug_enter())
  #print("Buffering matrix...")
  #print(m)
  
  # Check if input is numeric; this lets us specify the method for the buffering below for a small performance gain
  if (!is.numeric(m)) stop("Matrix is not numeric; only numeric matrices can be buffered.")
  
  # Buffer within columns
  m1 <- apply(m, 2, FUN = bufferSimple.numeric, width = width)
  #print(m1)
  
  # Buffer within rows; need to transpose because apply is weird
  m2 <- t(apply(m1, 1, FUN = bufferSimple.numeric, width = width))
  #print(m2)
  
  # Report and return
  message(message_debug_exit())
  return(m2)
  
}

# Buffer function that does not specify method for bufferSimple (for comparison)
bufferSimpleMatrixNoSpecMethod <- function(m, width = 1) {
  
  # Report
  message(message_debug_enter())
  #print("Buffering matrix...")
  #print(m)
  
  # Buffer within columns
  m1 <- apply(m, 2, FUN = bufferSimple, width = width)
  #print(m1)
  
  # Buffer within rows; need to transpose because apply is weird
  m2 <- t(apply(m1, 1, FUN = bufferSimple, width = width))
  #print(m2)
  
  # Report and return
  message(message_debug_exit())
  return(m2)
  
}

# Raster method for simple buffer
bufferSimple.Raster <- function(rst, width = 1) {
  
  # Report
  message(message_debug_enter())
  
  # Libraries
  library("raster")
  
  # Turn into matrix
  m <- as.matrix(rst)
  
  # Buffer
  mBuf <- bufferSimple(m, width = width)
  
  # Convert back to raster
  rstBuf <- raster(mBuf, template = rst)
  
  # Report and return
  message(message_debug_exit())
  return(rstBuf)
  
}


# More complicated buffer function that estimates euclidean distance
# Slower but more accurate

bufferEuclidean <- function(x, width = 1, twoStep = FALSE) UseMethod("bufferEuclidean")

bufferEuclidean.default <- function(x) {
  stop("Incompatible data type: must be numeric matrix or raster layer.")
}

# Matrix method for bufferEuclidean
bufferEuclidean.matrix <- function(m, width = 1, twoStep = FALSE) {
  
  # Report
  message(message_debug_enter())
  
  # Get xy values
  mx <- col(m)[]
  my <- row(m)[]
  
  # Get masked cells
  withMask <- which(m == 1)
  #print(withMask)
  
  # Simple twoStep
  # If twoStep, first call bufferSimple to turn values far outside to NA, reducing calc time for next steps
  if (twoStep == TRUE) {
    
    # Call simple buffer
    mBS <- bufferSimple(m, width = width)
    
    # Turn non-buffered area to NA
    #print(mBS)
    mBS[mBS != 1] <- NA
    #print(mBS)
    mx2 <- mx * mBS
    my2 <- my * mBS
    
  # Otherwise calc for whole matrix
  } else {
    
    mx2 <- mx
    my2 <- my
    
  }
  
  # For each masked cell
  mBuf <- m
  for (wm in withMask) {
    
    # Calculate xy components
    mxRel <- abs(mx2 - mx2[wm])
    myRel <- abs(my2 - my2[wm])
    
    # Calculate euclidean distance
    distRel <- sqrt(mxRel ^ 2 + myRel ^ 2)
    
    # Cutoff
    mBuf[distRel <= width] <- 1
    
  }
  
  # Report and return
  message(message_debug_exit())
  return(mBuf)
  
}

# Raster method for bufferEuclidean
bufferEuclidean.Raster <- function(rst, width = 1, twoStep = FALSE) {
  
  # Report
  message(message_debug_enter())
  #print("Buffering raster...")
  
  # Turn into matrix
  m <- as.matrix(rst)
  
  # Buffer
  mBuf <- bufferEuclidean(m, width = width, twoStep = twoStep)
  
  # Convert back to raster
  rstBuf <- raster(mBuf, template = rst)
  
  # Report and return
  message(message_debug_exit())
  return(rstBuf)
  
}


# minimum of the euclidean distance after being multiplied by original matrix, then mask if > buffer width
