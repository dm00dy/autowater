# Functions for manipulating f

# Requires conditions_base and input_testing

# Constructor function for a condition of (sub)class file_missing.  Signalled when a specified file cannot be found.
#
# Args:
#   text: Explanatory text to append to the condition message.
#  condClass (optional): the overall class (type) of condition this is.  Must be "error", "warning", or "message".
#     Defaults to "error".
#   trigger (optional): The condition that caused this condition to be signalled.  Appended to returned condition.
#     Defaults to NA.
#   ... (optional): Additional arguments to append to the constructed condition, providing more detail.
#
# Returns:
#   A condition object of classes name_of_function and condClass. Signalled when a specified file is missing.
file_missing <- function(text, condClass = "error", trigger = NA, ...) {
  
  msgBase <- "Specified file(s) missing"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}

# Constructor function for a condition of (sub)class file_unreadable.  Signalled when a specified file cannot be read.
# Defaults to error.
file_unreadable <- function(text, condClass = "error", trigger = NA, ...) {
  
  msgBase <- "Specified file(s) cannot be read"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}

# Constructor function for a condition of (sub)class file_unwritable.  Signalled when a specified file cannot be 
# written. Defaults to error.
file_unwritable <- function(text, condClass = "error", trigger = NA, ...) {
  
  msgBase <- "Specified file(s) cannot be written to disk"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}

# Constructor function for a condition of (sub)class file_undeletable.  Signalled when a specified file cannot be 
# deleted. Defaults to error.
file_undeletable <- function(text, condClass = "error", trigger = NA, ...) {
  
  msgBase <- "Specified file(s) cannot be deleted"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}

# Constructor function for a condition of (sub)class unspecified_directory.  Signalled when a specified file 
# does not have a directory.  Defaults to warning.
unspecified_directory <- function(text, condClass = "error", trigger = NA, ...) {
  
  msgBase <- "File(s) with unspecified directory"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}

# Constructor function for a condition of (sub)class semaphore_missing. Signalled when a specified file is missing
# an associated semaphore file indicating the processing and writing is complete. Defaults to error.
semaphore_missing <- function(text, condClass = "error", trigger = NA, ...) {
  
  msgBase <- "Specified file(s) missing an associated semaphore file"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}

# Constructor function for a condition of (sub)class extraction_failure. Signalled when file(s) cannot be 
# extracted as expected. Defaults to error.
extraction_failure <- function(text, condClass = "error", trigger = NA, ...) {
  
  msgBase <- "Specified file(s) cannot be extracted"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}

# Constructor function for a condition of (sub)class compression_failure. Signalled when file(s) cannot be 
# compressed as expected. Defaults to error.
compression_failure <- function(text, condClass = "error", trigger = NA, ...) {
  
  msgBase <- "Specified file(s) cannot be compressed and archived"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}

# Constructor function for a condition of (sub)class deletion_failure. Signalled when file(s) cannot be 
# deleted as expected. Defaults to error.
deletion_failure <- function(text, condClass = "error", trigger = NA, ...) {
  
  msgBase <- "Specified file(s) cannot be deleted"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}

###############
## Functions ##
###############

# Function to check if a file exists, optionally throwing an error if not.  Wrapper for file.exists that includes
# optional error generation.
#
# Args:
#   f: The name(s) of the file (as character) to the existence check of.
#  condClass (optional): The condition class (severity) to raise when an error is encountered.  Defaults to error.
#
# Returns:
#   TRUE if file exists, FALSE if not (unlesscondClass = "error", in which case execution is interrupted)
checkFile <- function(f, condClass = "error") {
  
  # Report
  message(message_debug_enter())
  
  # Check input
  checkInput(c("f", "condClass"), 
             classes = c("character", "character"), 
             lengths = c(NA, 1))
  
  message(message_trace("Checking file existence"))
  message(message_debug_vector("of files", f))
  
  # Check directories, warning if unspecified
  checkDir(f, condClass = "warning")
  
  # Check existence
  fe <- file.exists(f)
  message(message_debug_vector(paste("File(s) exist T/F:"), fe))
  
  # Throw warning/error as specified
  if (!all(fe)) {
    
    mf <- paste(f[!fe], collapse = ", ")
    
    raiseCondition(file_missing(mf, condClass, files = mf))

  }
  
  # Report and return
  message(message_debug_exit())
  return(fe)
  
}

# Function to get a file's directory, optionally throwing an error if unspecified.  Wrapper for dirname that
# includes optional error generation.
#
# Args:
#   f: The name(s) of the file (as character) to check.
#  condClass (optional): The condition class (severity) (severity) to raise if no directory is specified.  Defaults to error.
#     If not an error, will change to working directory.
#
# Returns:
#   A file's directory, unless contType == "error" and no directory is specified, then nothing.
checkDir <- function(f, condClass = "error") {
  
  # Report
  message(message_debug_enter())

  # Check input
  checkInput(c("f", "condClass"), 
             classes = c("character", "character"), 
             lengths = c(NA, 1))
  
  # Check directories
  dirs <- dirname(f)
  message(message_debug_vector("Path(s) specified:", dirs))
  
  # If missing, throw error if specified, else warn
  if (any(dirs == ".")) {
    
    ndf <- paste(f[dirs == "."], collapse = ", ")
    if (condClass != "error") ndf <- paste0(". Will assume working directory ", getwd())
    
    raiseCondition(unspecified_directory(paste0(ndf, "."),condClass, files = ndf))
    
  }
  
  # Report and return
  message(message_debug_exit())
  return(dirs)
  
}

# Function to write a file to disk; wrapper for normal file writing calls that includes error handling.
#
# Args:
#   writeFunction: The function to call to write the object to file.
#   condClass(optional): The class of the condition to raise if an error is encountered.  Defaults to error.
#   ... (optional): Additional arguments to writeFunction.  Technically optional, but required if you are going 
#     to actually write anything to file.
#
writeFile <- function(writeFunction, condClass = "error", ...) {
 
  # Report
  message(message_debug_enter())
  
  # Check input
  checkInput(c("writeFunction", "condClass"),
             classes = c("character", "character"),
             lengths = c(1, 1))
  
  
  # Find filename
  fn <- list(...)[grepl("file", names(list(...)))][1]
  
  # Write file (call writeFunction)
  message(message_trace(paste0("Writing file ", fn, "...")))
  ret <- tryCatch(match.fun(writeFunction)(...),
           error = function(e){
             
             
             raiseCondition(file_unwritable(paste0(fn, ", using function ", writeFunction, "."), 
                                            condClass, e, fxn = writeFunction, ...))
           
            })
   
  # Report and return
  message(message_debug_exit())
  return(ret)
  
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
gsutilCopy <- function(from, to, copySem = FALSE, semExt = ".sem", deleteFrom = FALSE, condClass = "error") {
  
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
    
    message(message_trace(paste("File successfully copied.")))
    
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

# Function to write a semaphore file indicating that the file has finished being processed and been completely
# written to disk.
# Returns:
#   TRUE if successful; otherwise throws error and returns nothing.
writeSemaphore <- function(f, semExt = ".sem", condClass = "error") {
  
  # Report
  message(message_debug_enter())
  
  # Check input
  checkInput(c("f", "semExt", "condClass"), 
               classes = c("character", "character", "character"), 
               lengths = c(NA, 1, 1))
  
  # Check base files (can't error now because need to write semaphores for files that did process)
  fe <- file.exists(f)
  message(message_debug_vector("Base file(s) exist:", fe))
  
  # Create semaphore filenames
  sfiles <- paste0(f, semExt)

  # Loop across files (don't use lapply because need to handle errors)
  message(message_trace("Writing semaphore file(s)..."))
  success <- rep(FALSE, length(f))
  errors <- list()
  for (fn in 1:length(f)) {
    
    f <- f[fn]
    sf <- sfiles[fn]
    
    # Write semaphore file if base file exists
    if (fe[fn]) {
      
      message(message_debug(paste0("Writing semaphore file ", sf)))
      msg <- paste0("Semaphore file marking the completion of file ", f, "; written at ", 
                    as.character(Sys.time()), ".")
      errors[[fn]] <- tryCatch({
                                write(msg, file = sf, append = FALSE)
                                success[fn] <- TRUE
                                NA
                              },
                              error = function(e) {
                                message(message_trace(paste("Failure in writing semaphore file", sf)))
                                success[fn] <<- FALSE
                                err <- file_unwritable(paste0("semaphore file ", sf), "error", e)
                                return(err)
                              })
      
    } else {
      
      message(message_debug(paste("Skipping writing semaphore file, as base file", f, "does not exist.")))
      
      # Delete existing semaphore file if it exists
      if (file.exists(sf)) {
        
        message(message_trace(paste("Deleting existing semaphore file", sf, "as base file", f, "does not exist.")))
        
        
      }
      
      success[fn] <- FALSE
      
    }
    
  }
  
  # Get unwritable semaphore f
  errorsTF <- vapply(errors, FUN = function(x) inherits(x, "file_unwritable"), FUN.VALUE = logical(1))
  message(message_debug(paste("Errors in writing file(s):", paste(errorsTF, collapse = ", "))))
  if (any(errorsTF)) {
    usf <- paste(sfiles[errorsTF], collapse = ", ")
  } else {
    usf <- character(0)
  }
  message(message_debug(paste("Unwritable semaphore file(s):", usf)))
  
  # If missing base file, throw error (regardless ofcondClass)
  if (!all(fe)) {
    
    # Warn if also failed in writing semaphore f
    if (any(errorsTF)) {
      warning(file_unwritable(paste0("semaphore file(s) ", usf), "warning", errors[errorsTF], files = usf))
    }
    
    mf  <- paste(f[!fe], collapse = ", ")
    mfMsg <- paste0(mf, ". Could not find expected base file(s) for which to write semaphore file(s); ", 
                    "previous processing must have failed!")
    raiseCondition(file_missing(mfMsg, "error", files = mf))
  
  # If failed in writing, throw error if requested, else warn
  } else if (any(errorsTF)) {
    
    raiseCondition(file_unwritable(paste0("semaphore file(s) ", usf),condClass, errors[errorsTF], files = usf))

  }
  
  # Report and return
  message(message_debug_exit())
  return(success)
  
}

# Function to check whether or not the passed files have an associated semaphore file marking the completion of
# processing and writing a file.
checkSemaphore <- function(f, semExt = ".sem", condClass = "error") {
  
  # Report
  message(message_debug_enter())
  
  # Check input
  checkInput(c("f", "semExt", "condClass"), 
             classes = c("character", "character", "character"), 
             lengths = c(NA, 1, 1))
  
  # Check if directories are specified
  checkDir(f,condClass = "warning")
  
  # Check if passed base f exist
  message(message_debug("Checking existence of base file(s)..."))
  fe <- file.exists(f)
  if (!all(fe)) {
    
    mf <- paste(f[!fe], collapse = ", ")
    mfMsg <- paste0(mf, ". Could not find expected base file(s) for which to check semaphore file(s);", 
                   " previous processing must have failed!")
    
    raiseCondition(file_missing(mfMsg,condClass, files = mf))
  
  }
  
  # Check if passed base files have semaphores
  message(message_debug("Checking existence of semaphore file(s)..."))
  sfiles <- paste0(f, semExt)
  sfe <- file.exists(sfiles)
  if (!all(sfe)) {
    
    msf <- paste(sfiles[!sfe], collapse = ", ")
    msfMsg <- paste0(msf, ". Previous processing must have failed!")
    
    raiseCondition(semaphore_missing(msfMsg,condClass, files = msf))

  }
  
  # Both f exist
  bfe <- fe & sfe
  message(message_debug(paste("Both files exist T/F:", paste(bfe, collapse = ", "))))
  
  # Report and return
  message(message_debug_exit())
  return(bfe)
  
}


# Function to extract the contents of a tarball to a directory
# Args:
#   f: The file to extract.
#   dest: The destination directory to extract to.  Must exist.
#   condClass (optional): The class of condition (error, warning, or message) to raise when an error 
#     is encountered.  Defaults to error.
#
# Returns:
#   Exit code from running tar.
extractTar <- function(f, dest, condClass = "error") {
  
  # Report
  message(message_debug_enter())
  
  # Check input
  checkInput(c("f", "dest", "condClass"), 
             classes = c("character", "character", "character"), 
             lengths = c(1, 1, 1))
  checkFile(f, condClass = "warning")
  checkDir(dest, condClass = "warning")
  
  # Invoke system command tar, extracting (-x) verbosely (-v) from (-f) file f to destination (-C) dest
  message(message_trace(paste0("Extracting file ", f, "...")))
  exitCode <- system2("tar", args = c("-xvf", f, paste("-C", dest)))
  raiseCondition(message_debug_vector("Exit code:", exitCode))
 
  # Parse exit code
  # If 0 (successful)
  if (exitCode == 0) {
    
    writeSemaphore(f, ".extracted")
    message(message_trace(paste("File successfully extracted.")))
    
  } else {
    
    msg <- paste0(f, ". Call to tar exited with a non-zero exit code (", exitCode, ").")
    raiseCondition(extraction_failure(msg,condClass, files = f, exitCode = exitCode))
    
  }
  
  # Report and return
  message(message_debug_exit())
  return(exitCode)
  
}

# Function to extract the contents of a tarball to a directory
# Args:
#   f: The file(s) to compress.
#   dest: The destination file to compress to.  Must NOT exist.
#   overwrite (optional): Whether or not to overwrite a pre-existing output; defaults to FALSE.
#   condClass (optional): The class of condition (error, warning, or message) to raise when an error 
#     is encountered.  Defaults to error.
#
# Returns:
#   Exit code from running tar.
createTar <- function(f, dest, overwrite = FALSE, condClass = "error") {
  
  # Report
  message(message_debug_enter())
  
  # Check input
  checkInput(c("f", "dest", "condClass"), 
             classes = c("character", "character", "character"), 
             lengths = c(NA, 1, 1))
  checkFile(f)
  de <- checkFile(dest, condClass = "message")
  if (de & overwrite != TRUE) {
    
    raiseCondition(file_unwritable("Destination file for createTar already exists!", condClass, file = dest))
  
  }
  
  # Check extension
  if (substr(dest, nchar(dest) - 6, nchar(dest)) != ".tar.gz") {
    
    raiseCondition(invalid_value("Extension of dest for createTar must be .tar.gz.  Changing...", "warning", file = dest))
    
  }
  
  # Invoke system command tar, creating (-c) and compressing (-z) verbosely (-v) an archive in destination dest 
  # without leading directory info (-C, change to current dir) containing file(s) f
  message(message_trace(paste0("Compressing file(s) ", paste(f, collapse = ", "), "...")))
  exitCode <- system2("tar", args = c("-czvf", dest, paste("-C", dirname(f), basename(f), collapse = " ")))
  raiseCondition(message_debug_vector("Exit code:", exitCode))
  
  # Parse exit code
  # If 0 (successful)
  if (exitCode == 0) {
    
    message(message_trace(paste("File successfully compressed.")))
    
  } else {
    
    msg <- paste0(f, ". Call to tar exited with a non-zero exit code (", exitCode, ").")
    raiseCondition(compression_failure(msg,condClass, files = f, exitCode = exitCode))
    
  }
  
  # Report and return
  message(message_debug_exit())
  return(exitCode)

}
  
# Function to delete the passed file(s) and their semaphore(s) if existing.
#
# Args:
#   f: The file(s) to delete.
#   deleteSem (optional): Whether or not to check for and delete associated semaphore files.  Defaults to FALSE.
#   semExt (optional): The semaphore extension to look for.  Defaults to .sem.
#   condClass (optional): The condition class (severity) (severity) to raise when an error is encountered.  Defaults to error.
#
deleteFile <- function(f, deleteSem = FALSE, semExt = ".sem", condClass = "error") {
  
  # Report
  message(message_debug_enter())
  
  # Check input
  checkInput(c("f", "deleteSem", "semExt", "condClass"), 
             classes = c("character", "logical", "character", "character"), 
             lengths = c(NA, 1, 1, 1))
  
  # Check for missing files
  fe <- checkFile(f, condClass = "warning")
  toDelete <- f[fe]
  message(message_debug_vector("File(s) exist T/F:", fe))
  
  # Check for associated semaphores
  if (deleteSem == TRUE) {
    
    message(message_trace("Checking existence of semaphore file(s)..."))
    sfiles <- paste0(f, semExt)
    se <- checkSemaphore(f, semExt, condClass = "warning")
    message(message_debug_vector("Semaphore file(s) exist T/F:", fe))
    toDelete <- c(toDelete, sfiles[se])
    
  }
  
  # Delete
  message(message_debug_vector("File(s) to delete:", toDelete))
  if (length(toDelete) == 0){
    
    msg <- paste0(f, ". None of the preceding file(s) exist.")
    raiseCondition(deletion_failure(msg, condClass, files = f))
    
  } else {
    
    message(message_trace("Deleting file(s)..."))
    tryCatch(file.remove(toDelete), 
           error = function(e) {
             
             msg <- paste0(toDelete, ". Unknown problem deleting one or more of the previous file(s).")
             raiseCondition(deletion_failure(msg,condClass, e, files = toDelete))
             
           })
  
  }
  
  # Report and return
  message(message_debug_exit())
  
}

