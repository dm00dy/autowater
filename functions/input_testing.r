# Function to check the class of the passed objects against a specified list.
# Designed for use in functions to test inputs.

# Dependencies: depends on the error_handling module (condition fxn) for customized error reporting
# Source error_handling.r

################
## Conditions ##
################

# Constructor for object_missing condition
object_missing <- function(text, condClass = "error", trigger = NA, ...) {
  
  msgBase <- "Required object missing"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}

# Constructor for invalid_length condition
invalid_length <- function(text, condClass = "error", trigger = NA, ...) {
  
  msgBase <- "Invalid vector length(s)"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}

# Constructor for invalid_class condition found in conditions_base

###############
## Functions ##
###############

# Function to return an object, throwing error if told, in an optionally specified environment (defaults to parent)
#
# Args:
#   objName: The name of the object to return.
#   envir (optional): The environment in which to search; defaults to the parent environemnt parent.frame().
#   condClass (optional): The class of condition (error, warning, or message) to raise when an object 
#     is missing; defaults to error.
#   fxnName (optional): The name of the function getObj is returning an object for;
#     used in error messaging; defaults to the name of the calling function.
#
# Returns:
#   Object objName if found; NULL if not found and condClass != "error".
getObj <- function(objName, envir = parent.frame(), condClass = "error", 
                   fxnName = as.character(sys.call(-1)[1])) {
  
  # Report
  message(message_debug_enter())
  
  # Check input
  if (!is.character(objName)) {
    stop(invalid_class("invalid object passed as string to getObj; objName must be a character vector.",
                       "error", object = objName, fxnName = fxnName))
  }
  if (length(objName) != 1) {
    objName <- objName[1]
    warning(invalid_length("getObj is not vectorized; only returning the first object (objName[1])."), 
            "warning", object = objName)
  }
  
  # Get object
  obj <- NULL
  tryCatch({
    
    obj <- get(objName, envir = envir)
    message(message_debug(paste0("Object '", objName, "' found.")))
    
    },
    
    # Object does not exist
    error = function(e) {
      
      msg <- paste0("object '", objName, "' does not exist in the specified environment of function ", fxnName, ".")
      
      # Signal condition
      raiseCondition(object_missing(msg, condClass, e, fxnName = fxnName, missingObject = objName,
                                     environment = envir))

    }
    
  )
  
  # Return
  message(message_debug_exit())
  return(obj)
  
}

# Function to test if argument(s) exist(s) in an optionally specified environment (defaults to parent)
#
# Args:
#   argExistNames: The name, or a vector of names, of the argument(s) (object) to test for.
#   envir (optional): The environment in which to search; defaults to the parent environemnt parent.frame().
#   condClass (optional): The class of condition (error, warning, or message) to raise when the argument 
#     is missing; defaults to warning.
#   fxnName (optional): The name of the function argumentExists is testing the existence of an argument for;
#     used in error messaging; defaults to the name of the calling function.
#
# Returns:
#   logical T/F vector the same length as argExistNames indicating whether each argument exists in the
#     specified environment.  Will not return anything if exits with an error (condClass = "error").
argumentExists <- function(argExistNames, envir = parent.frame(), condClass = "warning", 
                           fxnName = as.character(sys.call(-1)[1])) {
  
  # Report
  message(message_debug_enter())
  
  # Check that argExistNames is of class character
  if (!is.character(argExistNames)) {
    stop(invalid_class("invalid object passed as string to argumentExists; argExistNames must be a character vector.",
                       "error", fxnName = fxnName))
  }
  
  # Check existence
  argExists <- argExistNames %in% ls(envir = envir)
  message(message_debug_vector("Specified vars in environment:", ls(envir = envir)))
  message(message_debug_vector("Argument(s) exist T/F:", argExists))
  
  # Error/warn
  if (!all(argExists)) {
    
    ma <- argExistNames[!argExists]
    msg <- paste0("object(s) '", paste(ma, collapse = ", "), 
                  "' does not exist in the specified environment of function ", fxnName, ".")
    
    # Signal condition
    raiseCondition(object_missing(msg, condClass, fxnName = fxnName, missingObject = ma, environment = envir))
  
  }
  
  # Test if all are true and return
  message(message_debug_exit())
  return(argExists)
  
}

# Check input arguments
# 
# Args:
#   classes: The classes of the arguments to test. If NA, can be any class.
#   lengths (optional): The lengths of the arguments to test. If NA, can be any length. Defaults to NA.
#   condClass (optional): The class of condition (error, warning, or message) to raise when an object 
#     is missing; defaults to error.
# 
checkInput <- function(argNames, classes, lengths = rep(NA), 
                       fxnName = as.character(sys.call(-1)[1]), condClass = "error") {
  
  # Report
  message(message_debug_enter())
  message(message_debug_vector("Passed arg names:", argNames))
  
  # Check input to this function
  argumentExists(c("argNames", "classes", "lengths"), fxnName = fxnName)
  if (!is.character(argNames)) {
    stop(invalid_class("invalid object passed as string to checkInputs; argNames must be a character vector.", 
                       "error", fxnName = fxnName))
  }
  if (!isTRUE(all.equal(length(argNames), length(classes), length(lengths)))) {
    stop(error_vector_length("vectors passed to checkInputs (argNames, classes, lengths) must be of the 
                                      same length.", 
         fxnName = fxnName, argNamesLength = length(argNames), classesLength = length(classes)))
  }
  
  message(message_debug(paste0("Checking inputs to ", fxnName, "...")))
  
  # Check that the arguments specified in argNames were specified in the parent function's environment
  argExists <- argumentExists(argNames, envir = parent.frame(), condClass = condClass, fxnName = fxnName)
  
  # Check that the arguments exist and have the specified classes
  classesMatch <- c()
  for (an in 1:length(argNames)) {
    
    # Get values
    cl <- classes[an]
    argNm <- argNames[an]
    arg <- get(argNm, envir = parent.frame())
    argClass <- class(arg)
    
    # Report
    message(message_debug(paste0("Checking that the argument '", argNm, "' is of class ", cl)))
    message(message_debug(paste0("Argument '", argNm, "' class: ", paste(argClass, collapse = ", "))))
    message(message_debug(paste("Required class: ", cl)))
    
    # Check class
    if (is.na(cl) | cl %in% class(arg)) {
      
      classesMatch[an] <- TRUE
      message(message_debug(paste0("Argument '", argNm, "' has the correct class.")))
      
    } else {
      
      classesMatch[an] <- FALSE
      msg <- paste0("argument '", argNm, "' has an incorrect class.")
      
      # Signal appropriate condition
      raiseCondition(invalid_class(msg, condClass, fxnName = fxnName, 
                                   requiredClass = cl, actualClass = argClass))
 
    }
    
    # Get values
    len <- lengths[an]
    argLength <- length(arg)
    
    # Report
    message(message_debug(paste0("Argument '", argNm, "' length: ", argLength)))
    message(message_debug(paste("Required length: ", len)))
    message(message_debug(paste("Actual length: ", argLength)))
    
    # Check length if not NA
    lengthsMatch <- c()
    if (!is.na(len)) {
      
      message(message_debug(paste0("Checking that the argument '", argNm, "' is of length ", len)))
      
      # Check length
      if (argLength == len) {
        
        lengthsMatch[an] <- TRUE
        message(message_debug(paste0("Argument '", argNm, "' has the correct length.")))
        
      } else {
        
        classesMatch[an] <- FALSE
        msg <- paste0("argument '", argNm, "' has an incorrect length.")
        
        # Signal appropriate condition
        raiseCondition(invalid_length(msg, condClass, fxnName = fxnName, 
                                      requiredLength = len, actualLength = argLength))
        
      }
      
    } else {
      
      message(message_debug("Passed length is NA and so not comparing."))
      
    }
    
  }
  
  # Test if all tests passed and return
  message(message_debug_exit())
  return(all(argExists, classesMatch, lengthsMatch))
  
}
