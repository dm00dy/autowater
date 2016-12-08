# Functions for sourcing files
#
# nelliott, Oct 2015

# Requires nothing

# Function to source all files in a given directory
# Adapted from Sim on SO: http://stackoverflow.com/questions/12048436
# Args:
#   path: A character vector of the directory in which to look for source files.
#   pattern (optional): The file pattern (usually an extension) to match.  Defaults to R files ("\\.[rR]$").
#   recursive (optional): Whether or not to search recursively in the given directory.  Defaults to FALSE.
#
# Returns:
#   TRUE if successful; else throws error.
sourceDir <- function (path, pattern = "\\.[rR]$", recursive = FALSE) {
  
  # Check input
  if (class("path"))
  checkInput(argNames = c("path", "pattern", "recursive"), 
             classes = c("character", "character", "logical"), 
             lengths = c(NA, 1, 1), 
             fxnName = "sourceDir")
  message(message_trace(paste0("Sourcing files matching '", pattern, "' in ", path, ".")))
  
  # Get files
  files <- sort(list.files(path, pattern, full.names = TRUE, recursive = recursive))
  message(message_debug(paste("Files found:", paste(files, collapse = ", "))))
  
  # Load files
  if (length(files) == 0) {
    success <- FALSE
    warning(file_missing("no matching source files found in directory.", "warning", sourceDir = path))
  } else {
    success <- sourceFiles(files)
  }
  
  # Report and return
  message(message_debug_exit())
  return(success)
  
}

# Function to source a vector of passed files
#
# Args:
#   files: A character vector of files to source.
#
# Returns:
#   TRUE if successful; else throws error.
sourceFiles <- function(files) {
  
  # Report
  message(message_debug_enter())
  
  # Test inputs
  checkInput("files", "character")
  
  # Loop through and source files
  for (f in files) {
    
    # Check existence
    if (file.exists(f)) {
      
      message(message_trace(paste("Sourcing file", f)))
      
      # Source
      tryCatch(source(f),
               error = function(e) {
                 stop(sourcing_failure(paste0("could not source required file ", f, "."), "error", e, sourceFile = f))
               })

      
    } else {
      
      e <- file_missing(paste0("missing source file ", f, "."), "error", missingFile = f)
      stop(sourcing_failure(paste0("file ", f, " does not exist."), "error", e, sourceFile = f))
      
    }
    
  }
  
  # Report and return
  message(message_debug_exit())
  return(TRUE)
  
}
