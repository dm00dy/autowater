# Basic condition-related code, revolving around the constructor function for building custom conditions
# and a hierarchy of severity based on the C hierarchy of DEBUG, TRACE, INFO, WARN, ERROR, and FATAL.
#
# Condition constructor and functions based on Hadley Wickham's Advanced R book
#
# nelliott, June 2015

################################
## Base Condition Constructor ##
################################

# Function to construct a new object of class condition.  Is in turn used by specific constructor functions
# to create specific conditions.
#
# Args:
#   subclass: The new custom condition subclass to create.
#   condClass (optional): The type of condition to create; either error, warning, or message.  Defaults to error.
#   message (optional): The text to display when the condition is raised.  Defaults to a basic (uninformative) 
#     message.  Appended to the return condition.
#   trigger (optional): The condition that caused this condition to be signalled.  Defaults to NULL.
#   call (optional): The system call to display when the condition is raised.  Defaults to the previous call.
#     Appended to the returned condition.
#   ... (optional): Additional arguments to add to the returned condition.  Common additions include the name of 
#     the function that signalled the error and specific values related to the error.
#
# Returns:
#   A condition object of the subclass specified.
condition <- function(subclass = as.character(sys.call(-1)[1]), 
                      condClass = "error", 
                      msgBase = "Custom condition signalled",
                      msgDetail = paste0(subclass, " of type", condClass, "."), 
                      trigger = NA,
                      call = sys.call(-1), 
                      ...) {
  
  # Check condition type
  if (!(condClass %in% c("error", "warning", "message", "interrupt"))) {
    condClass <- "error"
    warning(invalid_class(paste("specified condition type (condClass, passed to condition()) invalid;",
                             "allowed types are error, warning, or message.  Setting as error."), "error"))
  }
  
  # Check base message
  if (!is.character(msgBase)) msgBase <- try(as.character(msgBase))
  if (is.tryerror(msgBase)) {
    warning(condition_construction_failure(paste0("could not build message for ", subclass, "."), 
                                           "warning", msgBase))
    msgBase <- "Custom condition signalled"
  }
  
  # Collapse msgDetail
  if (length(msgDetail) > 1) {
    warning(condition_construction_failure(paste0("improperly formatted message passed to condition construction of ", subclass, 
                                                  ". msgDetail should have only one element; collapsing."), "warning", msgDetail))
    msgDetail <- paste(msgDetail, collapse = ", ")
  }
  
  # Build message
  if (msgBase == msgDetail) {
    
    msg <- paste0(msgBase, "\n")
    
  } else {
    
    # Concatenate error message
    msg <- try(paste0(msgBase, ": ", msgDetail))
    if (is.condition(trigger)) {
      tm <- trigger$message
      msg <- try(paste0(msg, " ", toupper(substr(tm, 1, 1)), substr(tm, 2, nchar(tm)), ".\n"))
    } else {
      msg <- try(paste0(msg, "\n"))
    }
    
    # Error handling if concatenation failed
    if (is.tryerror(msg)) {
      warning(condition_construction_failure(paste0("could not build message for ", subclass, "."), 
                                             "warning", msg))
      msg <- msgBase
    } 
  }
  
  # Get time
  tm <- try(as.character(format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  if (is.tryerror(tm)) {
    warning(condition_construction_failure(paste0("could not get/format time for ", subclass, "."), "warning", msg))
    tm <- "UNKNOWN: could not get and/or format time."
  }
  
  # Construct condition
  cond <- structure(class = c(subclass, condClass, "condition"), 
    list(message = msg, time = tm, trigger = trigger, call = call, ...)
  )
  
  # Get severity
  if (is.null(cond$severity)) cond$severity <- conditionSeverity(cond)
  
  # Get call log
  if (is.null(cond$callLog)) cond$callLog <- sys.calls()
  
  # Return
  return(cond)
  
}

######################
## Condition Raiser ##
######################

# Function to raise a condition of the appropriate type
#
# Args: 
#   cond: The condition to raise.
raiseCondition <- function(cond) {
  
  if (is.condition(cond)) {
    
    if (is.error(cond)) {
      stop(cond)
    } else if (is.warning(cond)) {
      warning(cond)
    } else if (is.message(cond)) {
      message(cond)
    } else {
      warning("Condition of unknown class passed to raiseCondition.")
      message(cond)
    }
    
  } else {
    
    warning("Non-condition object passed to raiseCondition.")
    message(cond)
    
  }
  
}

##############################
## Condition Object Testing ##
##############################

# Function to test if an object is a condition
#
# Args:
#   x: The object to test.
#
# Returns:
#   A boolean value indicating whether or not the object is of class condition.
is.condition <- function(x) {

  inherits(x, "condition")
  
}

# Function to test if an object is an error
#
# Args:
#   x: The object to test.
#
# Returns:
#   A boolean value indicating whether or not the object is of class error.
is.error <- function(x) {
  
  inherits(x, "error")
  
}

# Function to test if an object is a warning
#
# Args:
#   x: The object to test.
#
# Returns:
#   A boolean value indicating whether or not the object is of class warning.
is.warning <- function(x) {
  
  inherits(x, "warning")
  
}

# Function to test if an object is a message
#
# Args:
#   x: The object to test.
#
# Returns:
#   A boolean value indicating whether or not the object is of class message.
is.message <- function(x) {
  
  inherits(x, "message")
  
}

# Function to test if an object is an interrupt
#
# Args:
#   x: The object to test.
#
# Returns:
#   A boolean value indicating whether or not the object is of class interrupt.
is.interrupt <- function(x) {
  
  inherits(x, "interrupt")
  
}

# Function to test if an object is a try-error
#
# Args:
#   x: The object to test.
#
# Returns:
#   A boolean value indicating whether or not the object is of class try-error.
is.tryerror <- function(x) {

  inherits(x, "try-error")
  
}

# Function to return the condition class (error, warning, or message)
#
# Args:
#   x: The condition object to test.
#
# Returns:
#   A string indicating the class of condition if it is a condition; otherwise an error.
#   Note that it will only match the most severe class of condition if there are multiple.
conditionClass <- function(cond) {
  
  if (!is.condition(cond)) {
    stop(invalid_class("This object is not a condition.", "error"))
  } else if (is.error(cond)) {
    return("error")
  } else if (is.warning(cond)) {
    return("warning")
  } else if (is.message(cond)) {
    return("message")
  } else if (is.interrupt(cond)) {
    return("error")
  } else {
    stop("This object is a condition of an unrecognized type: it is not an error, warning, or message.")
  }
  
}

# Function to get the condition subclass (i.e., more specific than error, warning, or message)
#
# Args:
#   x: The condition object to test.
#
# Returns:
#   A string indicating the subclass(es) of a condition if it is a condition; otherwise an error.
conditionSubclass <- function(cond) {
  
  if (!is.condition(cond)) {
    stop(invalid_class("This object is not a condition.", "error"))
  }
  
  return(class(cond)[!class(cond) %in% c("condition", "error", "warning", "message")])
  
}

# Function to change a condition's class
#
# Args:
#   cond: The condition object to change the class(es) of.
#   from: The condition 
#
#
# Returns:
#   The condition object with new class(es)
changeConditionClass <- function(cond, from, to) {
  
  if (!is.condition(cond)) {
    stop(invalid_class("This object is not a condition.", "error"))
  }
  
  # Change class
  class(cond) <- c(class(cond)[!(class(cond) %in% from)], to)
  
  # Change severity
  if (length(to) > 1) {
    to <- to[1]
    warning(invalid_length("multiple new classes were set by 'to' but only one severity can be set. Will add all
                           classes specified in 'to' and set severity to the first element only.", "warning"))
  }
  cond$severity <- to
  
  return(cond)
  
}

# Function to change an error to a warning (wrapper for changeConditionClass) that will optionally signal a warning.
errorToWarning <- function(cond, warn = FALSE) {
  
  cond <- changeConditionClass(cond, "error", "warning")
  if (warn == TRUE) warning(cond)
  return(cond)
  
}


###############################
## Condition Severity Levels ##
###############################

# Function that gives the order of condition severities.
#
# Args:
#   none
#
# Returns:
#   Ordered factor with the levels of condition severities.
conditionSeverityLevels <- function() {
  
  sevs <- factor(c("DEBUG", "TRACE", "INFO", "WARN", "ERROR", "FATAL"),
                 levels = c("DEBUG", "TRACE", "INFO", "WARN", "ERROR", "FATAL"), ordered = TRUE)
  return(sevs)
  
}

# Determine condition severity
conditionSeverity <- function(cond) {
  
  if (!is.condition(cond)) {
    stop(invalid_class("This object is not a condition.", "error"))
  }
  
  # Take only first element
  if (length(cond$severity) > 1) {
   cond$severity <- cond$severity[1]
    warning(invalid_length("condition has multiple severity levels set in condition$severity and only one is
                           allowed. Will use the first severity level only.", "warning"))
  }
  
  # If severity is properly set, get, factorize, and return
  if (!is.null(cond$severity)) {
    
    sev <- cond$severity
    
    if (sev %in% conditionSeverityLevels()) {
      sev <- factor(sev, levels = conditionSeverityLevels(), ordered = TRUE)
      return(sev)
    }
    
  }
  
  # If not, get condition's class and subclass
  condClass <- conditionClass(cond)
  condSubclass <- conditionSubclass(cond)
  
  # Evaluate severity based on observed class and subclass
  if (condClass == "error") {
    
    if ("error_fatal" %in% condSubclass) {
      sev <- "FATAL"
    } else {
      sev <- "ERROR"
    }
    
  } else if (condClass == "interrupt") {
    
    sev <- "FATAL"
    
  } else if (condClass == "warning") {
    
    sev <- "WARN"
    
  } else if (condClass == "message") {
    
    if ("message_trace" %in% condSubclass) {
      sev <- "TRACE"
    } else if ("message_debug" %in% condSubclass) {
      sev <- "DEBUG"
    } else {
      sev <- "INFO"
    }
    
  } else {
    
    sev <- "WARN"
    warning("Improper severity class set for condition object.")
    
  }
  
  # Factorize and return
  sev <- factor(sev, levels = conditionSeverityLevels(), ordered = TRUE)
  return(sev)
  
}

# Constructor function for message_debug, a subclass of the condition type message
# Used instead of print to report messages that may be useful for debugging
# 
# Args:
#   text: A character string to be added to the message's message text when signalled.  Should be informative.
#   call (optional): The call that led to this message being signalled; defaults to the grandparent call, as it
#     assumes this function will only (or almost always) be called from inside message() to actually signal a message.
#   ... (optional): Other arguments to pass to condition.
# 
# Returns:
#   A condition of class message_debug.
message_debug <- function(text, call = sys.call(-2), ...) {
  
  cond <- condition(condClass = "message", msgBase = text, msgDetail = text, call = call, ...)
  return(cond)
  
}

# Constructor function for message_debug_vector, a subclass of the condition type message
# Used instead of print to show the content of vectors
# 
# Args:
#   text: A character string to be added to the message's message text when signalled.  Should be informative.
#   vector: A vector of values to be added to the message text when signalled.
#   call (optional): The call that led to this message being signalled; defaults to the grandparent call, as it
#     assumes this function will only (or almost always) be called from inside message() to actually signal a message.
#   ... (optional): Other arguments to pass to condition.
# 
# Returns:
#   A condition of class message_debug_vector.
message_debug_vector <- function(text, vector, call = sys.call(-2), ...) {
  
  msg <- try(paste0(text, " ", paste(vector, collapse = ", "), "."))
  if (is.tryerror(msg)) {
    warning(condition_construction_failure("could not build message for message_debug_vector.", 
                                           "warning", msg))
    msg <- text
  } 
  cond <- condition(subclass = "message_debug", condClass = "message", msgBase = msg, msgDetail = msg, 
                    call = call, ...)
  return(cond)
  
}

# Constructor function for message_debug_enter, a subclass of the condition type message
# Placed in the beginning of custom functions to notify a user when a function is called; use to ease debugging.
# 
# Args:
#   fxnName (optional): The name of the function that is signalling the message.  Defaults to the grandparent.
#   call (optional): The call that led to this message being signalled; defaults to the grandparent call, as it
#     assumes this function will only (or almost always) be called from inside message() to actually signal a message.
#   ... (optional): Other arguments to pass to condition.
# 
# Returns:
#   A condition of class message_debug.
message_debug_enter <- function(fxnName = sys.call(-2)[1], ...) {
  
  msgBase <- "Entering function"
  cond <- condition(subclass = "message_debug", condClass = "message", msgBase = msgBase, 
                    msgDetail = fxnName, call = call, ...)
  return(cond)
  
}

# Constructor function for message_debug_exit, a subclass of the condition type message
# Placed in the end of custom functions to notify a user when a function done; use to ease debugging.
# 
# Args:
#   fxnName (optional): The name of the function that is signalling the message.  Defaults to the grandparent.
#   call (optional): The call that led to this message being signalled; defaults to the grandparent call, as it
#     assumes this function will only (or almost always) be called from inside message() to actually signal a message.
#   ... (optional): Other arguments to pass to condition.
# 
# Returns:
#   A condition of class message_debug.
message_debug_exit <- function(fxnName = sys.call(-2)[1], call = sys.call(-2), ...) {
  
  msgBase <- "Leaving function"
  cond <- condition(subclass = "message_debug", condClass = "message", msgBase = msgBase, 
                    msgDetail = fxnName, call = call, ...)
  return(cond)
  
}

# Constructor function for message_trace, a subclass of the condition type message
# Used instead of print to report messages that may be useful for tracing progress
# 
# Args:
#   text: A character string to be added to the message's message text when signalled.  Should be informative.
#   call (optional): The call that led to this message being signalled; defaults to the grandparent call, as it
#     assumes this function will only (or almost always) be called from inside message() to actually signal a message.
#   ... (optional): Other arguments to pass to condition.
# 
# Returns:
#   A condition of class message_trace.
message_trace <- function(text, call = sys.call(-2), ...) {
  
  cond <- condition(condClass = "message", msgBase = text, msgDetail = text, call = call, ...)
  return(cond)
  
}

# Constructor function for message_info, a subclass of the condition type message
# Placed in the end of custom functions to notify a user when a function done; use to report major info.
# 
# Args:
#   text: A character string to be added to the message's message text when signalled.  Should be informative.
#   call (optional): The call that led to this message being signalled; defaults to the grandparent call, as it
#     assumes this function will only (or almost always) be called from inside message() to actually signal a message.
#   ... (optional): Other arguments to pass to condition.
# 
# Returns:
#   A condition of class message_info.
message_info <- function(text, call = sys.call(-2), ...) {
  
  cond <- condition(condClass = "message", msgBase = text, msgDetail = text, call = call, ...)
  return(cond)
  
}

# Constructor function for error_fatal, a subclass of the condition type error.  Used to signal an error
# from which no recovery is possible.
# 
# Args:
#   text: A character string to be added to the message's message text when signalled.  Should be informative.
#   call (optional): The call that led to this message being signalled; defaults to the grandparent call, as it
#     assumes this function will only (or almost always) be called from inside message() to actually signal a message.
#   ... (optional): Other arguments to pass to condition.
# 
# Returns:
#   A condition of class error_fatal.
error_fatal <- function(text, call = sys.call(-2), ...) {
  
  msgBase <- "Fatal error encountered"
  cond <- condition(condClass = "error", msgBase = msgBase, msgDetail = fxnName, call = call, ...)
  return(cond)
  
}


#######################
## Custom Conditions ##
#######################

# Constructor function for a condition of (sub)class invalid_class.  Signalled when an object is not of the
# expected type.
#
# Args:
#   text: Explanatory text to append to the condition message.
#   condClass (optional): the overall class (type) of condition this is.  Must be "error", "warning", or "message".
#     Defaults to "error".
#   trigger (optional): The condition that caused this condition to be signalled.  Appended to returned condition.
#     Defaults to NA.
#   ... (optional): Additional arguments to append to the constructed condition, providing more detail.
#
# Returns:
#   A condition object of classes name_of_function and condClass.
invalid_class <- function(text, condClass = "error", trigger = NA, ...) {
  
  msgBase <- "Object has an invalid class"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}

# Constructor function for a condition of (sub)class invalid_value.  Signalled when an object has an invalid value.
invalid_value <- function(text, condClass = "error", trigger = NA, ...) {
  
  msgBase <- "Object has an invalid value"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}

# Constructor function for a condition of (sub)class condition_construction_failure.  Signalled when the constructor
# cannot build the custom condition as expected. Same args and returns as above.
condition_construction_failure <- function(text, condClass = "error", trigger = NA, ...) {
  
  msgBase <- "Problem building custom condition object"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}

