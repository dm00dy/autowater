# Error logging code
# Basic functions and classes to handle writing to a well-formatted error log
#
# nelliott, June 2015

# Require
# error handling
# input testing

# Libraries
library(tools)

################
## Conditions ##
################

# Constructor function to signal a condition encountered while writing a log.
#
# Args:
#   text: Explanatory text to append to the message.
#   condType (optional): the overall class (type) of condition this is.  Must be "error", "warning", or "message".
#     Defaults to "error".
#   trigger (optional): The condition that caused this condition to be signalled.  Appended to returned condition.
#     Defaults to NA.
#   ... (optional): Additional arguments to append to the constructed condition, providing more detail.
#
# Returns:
#   A condition object of classes name_of_function and condClass.
log_write_failure <- function(text, condClass = "error", trigger = NULL, ...) {
  
  msgBase <- "Problem writing to log"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}

##########################
## Formatting Functions ##
##########################

# Function to format a condition object as a string that can a. be written to file and b. easily read
#
# Args:
#   x: The condition object to be formatted.
#
# Returns:
#   Formatted condition as string if successful
conditionToString <- function(x) {
  
  # Report
  message(message_debug_enter())
  
  # Check input
  if (!is.condition(x)) {
    stop(invalid_class("invalid object passed to conditionToString; must be a condition.", "error", 
                       requiredClass = "condition", actualClass = class(x)))
  }
  
  # Convert and return
  text <- paste(unlist(x), rep("\n"), names(x), collapse = ": ")
  message(message_debug("Condition as text:", text))
  message(message_debug_exit())
  return(text)
  
}

# Function to format a pairlist as a string that can a. be written to file and b. easily read
# Developed to parse output of sys.calls()
#
# Args:
#   x: The pairlist to be formatted.
#
# Returns:
#   Formatted pairlist as string if successful
pairlistToString <- function(x) {
  
  # Report
  message(message_debug_enter())
  
  # Check input
  if (class(x) != "pairlist") {
    stop(invalid_class("invalid object passed to pairlistToString; must be a pairlist.", "error",
                       requiredClass = "pairlist", actualClass = class(x)))
  }
  
  # Convert and return
  headers <- paste0("[[", 1:length(x), "]]\n")
  text <- paste(headers, unlist(x), collapse = "\n")
  message(message_debug(paste("Headers as text:", paste(headers, collapse = ", "))))
  message(message_debug(paste("Pairlist as text:", text)))
  
  # Return
  message(message_debug_exit())
  return(text)
  
}

# Function to determine the depth of a list
# From http://stackoverflow.com/a/13433689/1270695
listDepth <- function(x, xdepth = 0) {
  if(is.na(x) | is.null(x) | !is.list(x)) {
    return(xdepth)
  } else {
    return(max(unlist(lapply(x, listDepth, xdepth = xdepth + 1))))    
  }
}

# Function to format a call as a string that can a. be written to file and b. easily read
# Developed to parse output of a call
#
# Args:
#   x: The call to be formatted.
#
# Returns:
#   Formatted call as string if successful
callToString <- function(x) {
  
  # Report
  message(message_debug_enter())
  
  # Check input
  if (class(x) != "call") {
    stop(invalid_class("invalid object passed to callToString; must be a call.", "error",
                       requiredClass = "call", actualClass = class(x)))
  }
  
  # Convert
  text <- paste0(deparse(x), collapse = "")
  tryCatch({
    text <- gsub("[ ]{2,}", " ", text)
    text <- gsub('"', "'", text)
  }, error = function(e) {
    warning("Problem using regular expressions in callToString to parse a call; returned un-regexed version.")
  })
  
  # Return
  message(message_debug(paste("Call as text:", text)))
  message(message_debug_exit())
  return(text)
  
}

# Function to nicely format a condition message in form [date time] SEVERITY message
#
# Args:
#   cond: The condition object to format.
#
# Returns:
#   Relevant info from condition formatted as a string
formatConditionMessage <- function(cond) {
  
  # Report
  message(message_debug_enter())
  
  # Check condition object
  if (!is.condition(cond)) {
    stop(invalid_class("invalid object passed as condition to formatConditionMessage; must be condition.", "error",
                       requiredClass = "condition", actualClass = class(cond)))
  }
  
  # Parse message
  if ("message" %in% names(cond)) {
    msg <- cond$message
  } else {
    msg <- "missing condition message."
  }
  
  # Strip newlines
  msg <- gsub("\\n", "", msg)
  
  # Parse time
  if ("time" %in% names(cond)) {
    tm <- cond$time
  } else {
    tm <- as.character(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  }
  message(message_debug(paste("Condition time as text:", tm)))
  
  # Get severity
  sev <- conditionSeverity(cond)
  message(message_debug(paste("Condition severity as text:", as.character(sev))))
  
  # Create message
  condMsg <- paste0("[", tm, "] ", sev, " -- ", msg)
  message(message_debug(paste("Condition message as text:", condMsg)))
  message(message_debug_exit())
  return(condMsg)
  
}

#######################
## Logging functions ##
#######################

# Function to report on the current condition (format nicely and output)
#
# Args:
#   cond: The condition object to format.
#   dest (optional): The place to send the formatted message; defaults to stderr()
#
# Returns:
#   Relevant info from condition formatted as a string
outputCondition <- function(cond, dest = stderr()) {
  
  # Report
  message(message_debug_enter())
  
  # Format and output
  msg <- formatConditionMessage(cond)
  write(msg, dest)
  
  # Reutrn
  message(message_debug(paste("Condition text", msg)))
  message(message_debug_exit())
  return(msg)
  
}

# Function to write to a log file as csv.  Inputs must be well-formatted and additional values to be written 
# to log must match the format of the existing log.
#
# Args:
#   values: A vector of values to write to a log. Must be coercable to numeric or character (no lists!).
#   colNames: A vector of the column names corresponding to the values passed.  Must match in length.
#   logPath (optional): The path where the log file resides; defaults to the working directory.
#   logFilename (optional): The filename to write the log to; defaults to error_log.csv.  Must be a csv file.
#
# Returns:
#   TRUE if successful
logWrite <- function(values, 
                     colNames, 
                     colTypes = NULL, 
                     logPath = getwd(),
                     logFilename = "error_log.csv"
                     ) {
  
  # Report
  message(message_debug_enter())
  
  # Check existence of path
  if (!file.exists(logPath)) {
    warning(paste0("Error log path specified in call to logWrite (", logPath, ") does not exist. Changing to 
                  working directory (", getwd(), ")."))
    logPath <- getwd()
  }
  
  # Check number of values agains number of colnames
  if (length(values) != length(colNames)) {
    warning(invalid_length("Mismatch in number of column names vs number of values passed to logWrite.", 
                           "warning", lengthValues = length(values), lengthColNames = length(colNames)))
  }
  
  # Fix log filename
  if (!is.character(logFilename)) {
    logFilename <- "error_log.csv"
    warning("Invalid filename passed to logWrite; changed to default of error_log.csv.")
  }
  
  # Fix extension
  if (file_ext(logFilename) != "csv") {
    logFilename <- paste0(logFilename, ".csv")
    warning("Changed log filename (logFilename; passed to logging function logWrite) extension to csv.")
  }
  
  logFile <- file.path(logPath, logFilename)
  logFileBackup <- file.path(logPath, paste0(substr(logFilename, 1, nchar(logFilename) - 4), "_backup.csv"))
  
  # Create df
  logEntry <- matrix(values, ncol = length(colNames), byrow = TRUE)
  logEntryDf <- data.frame(logEntry)
  names(logEntryDf) <- colNames
  
  # If log file exists, grab to append
  if (file.exists(logFile)) {
    
    # Backup
    tryCatch(file.copy(logFile, logFileBackup, overwrite = TRUE),
             error = function(e) {
               warning(paste0("Could not copy existing log file to", logFileBackup, ".  ", e))
              }
             )
    
    # Read and combine
    logDf <- read.csv(logFile)
    lDf <<- logDf
    leDf <<- logEntryDf
    tryCatch(logDf <- rbind(logDf, logEntryDf),
             error = function(e) {
               stop(log_write_failure("format of new log entry did not match existing log format (rbind).", 
                                      "error", e, newNames = names(logEntryDf), oldNames = names(logDf)))
              }
             )
    
  # Else make new
  } else {
    
    logDf <- logEntryDf
    
  }
  
  # Write
  tryCatch(write.csv(logDf, file = logFile, row.names = FALSE),
           error = function(e) {
              stop(log_write_failure("could not write log to file (write.csv).", e))
            }, 
           warning = function(w) {
             warning(log_write_failure("warning in writing log to file (write.csv)", w))
           }
           )
  
  # Return
  message(message_debug_exit())
  return(TRUE)
  
}

# Function that parses a condition object and writes it to a columnar log
# 
# Args:
#   cond: A condition object to write to log.
#   ... (optional): Other arguments to logWrite (e.g., logPath, logFilename)
#
# Returns:
#   Return value from logWrite (TRUE if successful)
logWriteCondition <- function(cond, ...) {
  
  # Report
  message(message_debug_enter())
  
  # Check condition object
  if (!is.condition(cond)) {
    stop(invalid_class("invalid object passed as condition to logWriteCondition; must be condition.", "error",
                       requiredClass = "condition", actualClass = class(cond)))
  }
  
  # Parse condition class
  condClass <- conditionClass(cond)
  condSubclass <- conditionSubclass(cond)
  
  # Parse message
  msg <- conditionMessage(cond)
  if (is.null(msg)) msg <- NA
  
  # Parse trigger
  if ("trigger" %in% names(cond)) {
    trigger <- cond$trigger
    if (is.condition(trigger)) {
      trigger <- conditionToString(trigger)
    } else {
      trigger <- as.character(trigger)
    }
  } else {
    trigger <- NA
  }
  
  # Parse call
  call <- conditionCall(cond)
  if ("call" %in% class(call)) {
    call <- callToString(call)
  } else if (is.null(call)) {
    call <- NA
  } else {
    call <- as.character(call)
  }
  cnd <<- cond
  
  # Format condition
  condText <- conditionToString(cond)
  
  # Format callLog
  if ("callLog" %in% names(cond)) {
    callLog <- cond$callLog
    if (is.pairlist(callLog)) {
      callLogText <- pairlistToString(callLog)
    } else {
      callLogText <- ""
    }
  } else {
    callLogText <- ""
  }
  
  # Format datetime
  dtFmt <- as.character(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  
  # Concatenate values
  values <- c(dtFmt, as.character(condClass), as.character(condSubclass), 
              as.character(msg),
              as.character(trigger),
              condText, 
              call,
              callLogText)
  colNames <- c("Time", "ConditionClass", "ConditionSubclass", "Message", "Trigger",
                "Condition", "PreviousCall", "CallLog")
  
  # Write log
  logWrite(values, colNames, ...)
  
}

# Function to log a condition
# Write (and return) a well-formated message (formatConditionMessage) to stdout as
# well as write details of the condition (including call log) to a more detailed columnar log
# 
# Args:
#   cond: A condition object to format, return, and write to log.
#   dest (optional): The place to output logging messages to; defaults to stderr().
#   toDest (optional): Whether or not to output the logging message to; defaults to TRUE.
#   toFile (optional): Whether or not to write to file or just output it to dest; defaults to TRUE.
#   debugLogger (optional): Whether or not to run the logger in debug mode, printing messages generated
#     from within the logger functions.  Defaults to FALSE.
#   ... (optional): Other arguments to logWriteCondition (e.g., logPath, logFilename)
#
# Returns:
#   TRUE if successful
logCondition <- function(cond, dest = stderr(), toDest = TRUE, toFile = TRUE, debugLogger = FALSE, ...) {
  
  # Get condition severity
  sev <- conditionSeverity(cond)
  
  # Output the condition
  if (toDest == TRUE) {
    
    withCallingHandlers(outputCondition(cond, dest = dest),
                        
                        error = function(e) {
                          stop("Error writing to console.")
                        },
                        
                        warning = function(w) {
                          if (debugLogger == TRUE) write(w$message, dest)
                          invokeRestart("muffleWarning")
                        },
                        
                        message = function(m) {
                          if (debugLogger == TRUE) write(m$message, dest)
                          invokeRestart("muffleMessage")
                        }
    
    )
  
  }
  
  # Write condition to a log file
  if (toFile == TRUE) {
    
    withCallingHandlers(logWriteCondition(cond, callLog, ...),
                        
                         error = function(e) {
                           outputCondition(e, dest)
                           stop(log_write_failure("unexpected error in logWriteCondition().", e, "error"))             
                         }, 
                         
                         warning = function(w) {
                           outputCondition(w, dest)
                           invokeRestart("muffleWarning")           
                         }, 
                         
                         message = function(m) {
                           if(debugLogger == TRUE) outputCondition(m, dest)
                           invokeRestart("muffleMessage")
                         }
    )
    
  }

  return(TRUE)
  
}

# Function to call
my_log <- function(x) {
  
  message(message_debug_enter())
  
  if (!is.numeric(x)) stop(invalid_class("invalid class passed to my_log"))
  if (any(x < 0)) stop(invalid_value("invalid value passed to my_log"))
  
  message(message_debug_exit())
  return(log(x))
  
}
