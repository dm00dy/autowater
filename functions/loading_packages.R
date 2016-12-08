# Functions to check, load, and install packages
#
# nelliott, Nov 2015

################
## Conditions ##
################

# Constructor function for a condition of (sub)class package_missing.  Signalled when a required R package is 
# expected to be installed but missing.
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
package_missing <- function(text, condClass = "error", trigger = NA, ...) {
  
  msgBase <- "Required package(s) is/are missing"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}

# Constructor function for a condition of (sub)class url_failure.  Signalled when a url connection attempt fails.
url_failure <- function(text, condClass = "error", trigger = NA, ...) {
  
  msgBase <- "Could not open URL"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}

# Constructor function for a condition of (sub)class invalid_package.  Signalled when a specified package name
# is invalid (doesn't exist).
invalid_package <- function(text, condClass = "error", trigger = NA, ...) {
  
  msgBase <- "Specified package name is invalid"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}

###############
## Functions ##
###############

# Function to test if a string is a valid (openable) url address
#
# Args:
#   x: The string to test.
#   throwError (optional): Whether or not to throw an error if the URL cannot be opened; defaults to FALSE.
#
# Returns:
#   If throwError == FALSE, logical value indicating the existence of the URL.
#   If throwError == TRUE, TRUE if URL exists, nothing (throws error) if it does not.
urlExists <- function(x, throwError = FALSE) {
  
  # Report
  message(message_debug_enter())
  
  # Test input
  if (!is.character(x)) {
    stop(error_invalid_class("url passed to urlExists must be a string."))
  }
  if (length(x) != 1) {
    warning(warning_invalid_length("urlExists is not vectorized; will only test first passed URL. "))
  }
  
  # Check existence
  exists <- NULL
  tryCatch({
    
    conn <- url(x)
    open(conn)
    
    exists <- TRUE
    message(message_trace("URL exists."))  
    
  }, error = function(e) {
    
    exists <<- FALSE
    msg <- paste("URL", x, "cannot be opened.")
    
    # Throw error if requested
    if (throwError == TRUE) {
      stop(url_failure(msg, "error", e))
      
    # Else warn
    } else {
      warning(url_failure(msg, "warning", e))
    }
    
  }, finally = {
    
    # Close connection
    try(close(conn), silent = TRUE)
    
  })
  
  # Report and return
  message(message_debug_exit())
  return(exists)
  
}

# Function to check whether or not the passed packages are installed.  May optionally install if missing
# and/or load packages.
#
# Args:
#   pkgs: The package(s) to test the installation of.
#   installIfMissing (optional): Whether or not to install any packages that are missing. Defaults to FALSE. 
#   repos (optional): The R repository from which to install missing packages. Only used if installIfMissing = TRUE.
#     Defaults to the main CRAN repository (http://cran.us.r-project.org).
#   loadPackages(optional): Whether or not to load the packages being checked for.  Defaults to FALSE.
#
# Returns:
#   A logical vector indicating whether or not the specified packages are installed. Function will fail to return
#     installIfMissing is set to TRUE and the missing package(s) cannot be installed.
packagesInstalled <- function(pkgs, installIfMissing = FALSE, repos = "http://cran.us.r-project.org", 
                              loadPackages = FALSE) {
  
  # Report
  message(message_debug_enter())
  message(message_debug(paste("Checking installation of packages", paste(pkgs, collapse = ", "))))
  
  # Check input
  checkInput(argNames = c("pkgs", "installIfMissing", "repos", "loadPackages"), 
             classes = c("character", "logical", "character", "logical"), 
             lengths = c(NA, 1, 1, 1), 
             fxnName = "packagesInstalled")
  validUrl <- urlExists(repos, throwError = installIfMissing)
  
  # Get installed packages
  iPkgs <- unname(installed.packages()[,"Package"])
  
  # Check packages
  installed <- pkgs %in% iPkgs
  message(message_debug(paste("Installed T/F:", paste(installed, collapse = ", "))))
  
  # If missing
  if (!all(installed)) {
    
    mPkgs <- pkgs[installed == FALSE]
    message(message_info(paste("The following package(s) are not installed:", paste(mPkgs, collapse = ", "))))
    
    # Install if requested
    if (installIfMissing == TRUE & validUrl == TRUE) {
      
      message(message_info("Installing missing packages..."))
      
      # Loop across missing packages
      for (mp in mPkgs) {
          
        message(message_trace(paste("Installing missing package", mp)))
        
        # Check the package exists
        if (mp %in% data.frame(available.packages())$Package) {
          
          tryCatch(install.packages(mp, repos = repos),
                   error = function(e) {
                     
                     stop(package_missing(paste("missing package", mp, "could not be installed."),
                                               "error", e, missingPackages = mp))
                   
                   })
          
        } else {
          
          stop(invalid_package(paste0("package ", mp, " does not exist in the CRAN repository. 
                                      Please check spelling."), "error", invalidPackage = mp))
          
        }
          
      }
      
      # Update installed package list
      iPkgs <- unname(installed.packages()[,"Package"])
      installed <- pkgs %in% iPkgs
      
    } else {
      
      stop(package_missing(paste("missing required package(s) ", paste(mPkgs, collapse = ", "), ".", sep = ""), 
                                  "error", missingPackages = mPkgs))
    
    }
    
  # Otherwise all is good
  } else {
    
    message(message_info("All specified packages installed."))
    
  }
  
  # Load if requested
  if (loadPackages == TRUE) {
    
    message(message_trace("Loading packages..."))
    
    for (pkg in pkgs) {
      
      require(pkg)
      message(message_trace(paste("Loading package", pkg)))
      
    }
  
  }
  
  # Report and return
  message(message_debug_exit())
  return(installed)
  
}