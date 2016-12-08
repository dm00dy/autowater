# Functions for manipulating f

# Requires conditions_base, input_testing, and file_manipulation


################
## Conditions ##
################

# Constructor function for a condition of (sub)class gs_unmounted.  Signalled when the specified google storage
# is not mounted.  Defaults to error.
#
# Args:
#  text: Explanatory text to append to the condition message.
#  condClass (optional): the overall class (type) of condition this is.  Must be "error", "warning", or "message".
#     Defaults to "error".
#   trigger (optional): The condition that caused this condition to be signalled.  Appended to returned condition.
#     Defaults to NA.
#   ... (optional): Additional arguments to append to the constructed condition, providing more detail.
#
# Returns:
#   A condition object of classes name_of_function and condClass.
gs_unmounted <- function(text, condClass = "error", trigger = NA, ...) {
  
  msgBase <- "Specified google storage is unmounted"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}



###############
## Functions ##
###############

# Function to check if the google drive buckets are mounted, optionally trying to mount if not
#
# Args:
#   buckets (optional): The path of the bucket(s) to check; defaults to cdr, pub, and pvt.
#   condClass (optional): The class of condition to raise if the google storage bucket(s) are unmounted. 
#     Defaults to error.
#   mount (optional): Whether or not to try to mount the google storage buckets if they are unmounted.
#   ... (optional): Additional arguments to gsMount.
#
# Returns:
#   TRUE if mounted; FALSE otherwise.
gsCheck <- function(buckets = file.path("/home/blue", c("cdr", "pub", "pvt")), 
                    condClass = "error", mount = FALSE, ...) {
  
  # Report
  message(message_debug_enter())
  
  # Check input
  checkInput(c("buckets", "condClass", "mount"),
             classes = c("character", "character", "logical"),
             lengths = c(NA, 1, 1))
  
  # Check if mounted
  fe <- file.exists(file.path(buckets, paste0(basename(buckets), ".root")))
  raiseCondition(message_debug_vector("Buckets mounted (root files exist)?", fe))
  
  if (all(fe)) {
    
    raiseCondition(message_info("Specfied buckets are all mounted."))
    mounted <- TRUE
    
  } else {
    
    mounted <- FALSE
    
    # Try to mount if directed
    if (mount == TRUE) {
      
      raiseCondition(message_info("One or more buckets unmounted.  Attempting to mount..."))
      exitCode <- gsMount(condClass = "warning", ...)
      raiseCondition(message_debug_vector("Exit code", exitCode))
      
      # Check again
      mounted <- gsCheck(buckets, condClass, mount = FALSE)
      
    } else {
      
      msg <- "and was not mounted."
      raiseCondition(gs_unmounted(msg, condClass, buckets = buckets))
      
    }
    
  }
  
  # Report and return
  message(message_debug_exit())
  return(mounted)
    
}

# Function to mount the google drive buckets if they are not mounted
gsMount <- function(mountScript = "/home/blue/mountall", condClass = "error") {
  
  # Report
  message(message_debug_enter())
  
  # Check input
  checkInput(c("mountScript", "condClass"),
             classes = c("character", "character"),
             lengths = c(1, 1))
  
  # Check file
  checkFile(mountScript)
  
  # Call bash to run mounting script
  raiseCondition(message_trace(paste0("Calling bash to run mounting script ", mountScript, "...")))
  exitCode <- system2("bash", args = c("-x", mountScript))
  raiseCondition(message_debug_vector("Exit code:", exitCode))
  
  # If 0 (successful)
  if (exitCode == 0) {
    
    #exitCode <- gsCheck()
    if (exitCode == 0) {
      message(message_info("Google storage buckets successfully mounted."))
    }
    

  } else {
    
    msg <- paste0(from, "and could not be mounted. Call to bash running mount script exited non-zero exit code (", 
                  exitCode, ").")
    raiseCondition(gs_unmounted(msg, condClass, files = to, exitCode = exitCode))
    
  }
  
  # Report and return
  message(message_debug_exit())
  return(exitCode)
  
}

# Function to copy a file on linux using gsutil to a google cloud bucket.  Needed because writing directly seems
# to fail silently.
#
# Args:
#   from: The file to copy.
#   to: The destination to copy the file to.
#   copySem (optional): Whether or not to copy the associated semaphore file. Defaults to FALSE.
#   semExt (optional): The extension of the semaphore file to copy. Defaults to ".sem".
#   deleteFrom (optional): Whether or not to delete file from after a successful copy. If copySem is TRUE, will also
#     delete associated semaphore file. Defaults to FALSE.
#   condClass (optional): The class of the condition to raise if an error is encountered. Defaults to error.
#
gsCopy <- function(from, to, copySem = FALSE, semExt = ".sem", deleteFrom = FALSE, condClass = "error") {
  
  # Report
  message(message_debug_enter())
  
  # Check input
  checkInput(c("from", "to", "copySem", "condClass"),
             classes = c("character", "character", "logical", "character"),
             lengths = c(1, 1, 1, 1))
  
  # Invoke system command gsutil
  message(message_trace(paste0("Calling gsutil to copy file (", from, " to ", to, "...")))
  exitCode <- system2("gsutil", args = c("-q", "cp", from, to))
  raiseCondition(message_debug_vector("Exit code:", exitCode))
  
  # Parse exit code
  # If 0 (successful)
  if (exitCode == 0) {
    
    message(message_trace("File successfully copied."))
    
    # Copy semaphore too if desired
    exitCodeSem <- 0
    if (copySem == TRUE) {
      message(message_trace(paste0("Copying semaphore file ", from, " to ", to, "...")))
      exitCodeSem <- system2("gsutil", args = c("-q", "cp", paste0(from, semExt), dirname(to)))
      raiseCondition(message_debug_vector("Exit code (for sem):", exitCodeSem))
    }
    
    # Delete from file(s) if indicated
    if (deleteFrom == TRUE) {
      
      message(message_trace("Removing original file(s)..."))
      deleteFile(from, deleteSem = copySem, semExt = semExt, condClass = "warning")
      
    }
    
  } else {
    
    msg <- paste0(from, ". Call to gsutil exited non-zero exit code (", exitCode, ").")
    raiseCondition(file_unwritable(msg, condClass, files = to, exitCode = exitCode))
    
  }
  
  # Report and return
  message(message_debug_exit())
  return(exitCode + exitCodeSem)
  
}
