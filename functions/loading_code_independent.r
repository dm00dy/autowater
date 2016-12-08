# Functions for sourcing files
#
# nelliott, Oct 2015

# Requires nothing

# Format condition message
fmtMsg <- function(msg, sev = "INFO") {
  
  tm <- as.character(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  msg <- paste0("[", tm, "] ", sev, " -- ", msg)
  
}

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
  
  message(fmtMsg(paste("Sourcing all R files in", path)))

  # Get files
  files <- sort(list.files(path, pattern, full.names = TRUE, recursive = recursive))
  #message(fmtMsg(paste("Files found:", paste(files, collapse = ", "))))
  
  # Load files
  if (length(files) == 0) {
    success <- FALSE
    warning("No matching source files found in directory.")
  } else {
    success <- sourceFiles(files)
  }
  
  # Report and return
  #message("Leaving function sourceDir.")
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
  
  #message("Sourcing files...")
  
  # Loop through and source files
  for (f in files) {
    
    # Check existence
    if (file.exists(f)) {
      
      #message(fmtMsg(paste("Sourcing file", f)))
      
      # Source
      tryCatch(source(f),
               error = function(e) {
                 stop(fmtMsg(paste0("Could not source required file ", f, "."), "ERROR"))
               })

      
    } else {
      
      stop(fmtMsg(paste("File", f, "does not exist."), "ERROR"))
      
    }
    
  }
  
  # Report and return
  return(TRUE)
  
}
