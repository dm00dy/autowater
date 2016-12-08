# Script to run autowater functions
#
# cd C:\Program Files\R\R-3.1.0\bin\
# Rscript.exe C:\data\sparklemotion\autowater\testing\run_autowater_function.r
# Rscript /home/blue/sparklemotion/autowater/scripts/run_autowater_function.r
# Rscript /home/blue/sparklemotion/autowater/scripts/run_autowater_function.r run standardizeLandsatRasters --dateStart "2014-01-01" --dateEnd "2014-03-01"

# Add ability to specify path and row

# Define command line usage
"Function to run autowater R functions from command line.

Usage: 
  run_autowater_function run <fxn> [options]
  run_autowater_function -h | --help

Options:
  -h, --help     Show this screen.
  --scenes=<scn>         Scenes to process, using format p44r34 or 044034, separate multiple entries with commas; defaults to the four central valley scenes.
  --dateStart=<ds>       First date of files to process, using format yyyy-mm-dd; defaults to today.
  --dateEnd=<de>         First date of files to process, using format yyyy-mm-dd; defaults to today.
  --logOutLevel=<lol>    Minimum condition level to output to console [default: TRACE].
  --baseDir=<bd>         Base directory of all local files; requires specific directory structure [default: /home/blue].
  --codeDir=<cd>         Directory that contains the code files to source; defaults to <baseDir>/sparklemotion/autowater/functions).
  --tempDir=<td>         Directory to store temporary files that will be deleted in session; defaults to <baseDir>/processing).
  --overwrite=<ov>       Whether or not to overwrite existing output; defaults to false.

Arguments:
  <fxn>  Name of R function to run; must be loaded when sourcing codeDir." -> doc

# Load docopt package and get options
library(docopt)
opts <- docopt(doc)
#print(opts)

# Check baseDir exists
if (!file.exists(opts$baseDir)) stop(paste("Specified base directory", opts$baseDir, "does not exist. Exiting."))

# Check codeDir exists
if (is.null(opts$codeDir)) opts$codeDir <- file.path(opts$baseDir, "sparklemotion/autowater/functions")
if (!file.exists(opts$codeDir)) stop(paste("Specified code directory", opts$codeDir, "does not exist. Exiting."))

# Load code
source(file.path(opts$codeDir, "loading_code_independent.r"))
allSourced <- sourceDir(opts$codeDir)
if (!allSourced) stop(paste0("Could not successfully load all code in ", opts$codeDir, ". Exiting."))

# Check R function exists
rFxn <- try(match.fun(opts$fxn), silent = TRUE)
if (inherits(rFxn, "try-error"))  {
  stop(paste0("Function ", opts$fxn, " cannot be loaded, likely because it does not exist in the current environment ",
              "after sourcing codeDir ", opts$codeDir, ". Exiting."))
}

# Check tempDir exists
if(is.null(opts$tempDir)) opts$tempDir <- file.path(opts$baseDir, "processing")
if (!file.exists(opts$tempDir)) stop(paste("Specified temp directory", opts$tempDir, "does not exist. Exiting."))

# Check/set scenes
if (is.null(opts$scenes)) {
  
  opts$scenes <- c("p42r35", "p43r34", "p44r33", "p44r34")
  message("As scenes were not specified, defaulting to four Central Valley scenes.")
  
} else {
  
  scenes <- unlist(strsplit(opts$scenes, ",", fixed = TRUE))
  
  if (all(substr(scenes, 1, 1) == "p") & all(substr(scenes, 4, 4) == "r")) {
    
    message("Scene format: p44r34")
    scenes <- paste0("0", substr(scenes, 2, 3), "0", substr(scenes, 5, 6))
    
  } else if (all(grepl("[0-9]{6}", scenes))) {
    
    message("Scene format: 044034")
    if (!(all(substr(scenes, 1, 1) == 0 & substr(scenes, 4, 4) == 0))) {
      warning("scenes with paths or rows in the hundreds may not be returned if files are stored in format p44r34.")
    }
    
  } else {
    
    stop("Invalid scene format: scenes must either be formatted as p44r34 or 044034.")
    
  }
  
  opts$scenes <- scenes
  
}

# Check/set start date
if (is.null(opts$dateStart)) {
  
  opts$dateStart <- format(Sys.time(), format = "%Y-%m-%d")
  message("As dateStart was not set, setting dateStart to current date.")
  
} else {
  
  opts$dateStart <- try(format(as.Date(opts$dateStart), format = "%Y-%m-%d"), silent = TRUE)
  if (inherits(opts$dateStart, "try-error"))  {
    stop(paste0("Given dateStart ", opts$dateStart, " cannot be converted into a valid date. Check that it is in a", 
                "standard unambiguous date format.", "Exiting."))
  }
  
}

# Check/set end date
if (is.null(opts$dateEnd)) {
  
  opts$dateEnd <- format(Sys.time(), format = "%Y-%m-%d")
  message("As dateEnd was not set, setting dateEnd to current date.")
  
} else {
  
  opts$dateEnd <- try(format(as.Date(opts$dateEnd), format = "%Y-%m-%d"), silent = TRUE)
  if (inherits(opts$dateEnd, "try-error"))  {
    stop(paste0("Given dateEnd ", opts$dateEnd, " cannot be converted into a valid date. Check that it is in a", 
                "standard unambiguous date format.", "Exiting."))
  }
  
}


# Check/set overwrite flag
if (is.null(opts$overwrite)) {
  
  opts$overwrite <- FALSE
  message("As overwrite was not specified, setting overwrite to FALSE.")
  
}

# Run function
rFxn(baseDir = opts$baseDir, scenes = opts$scenes, dateStart = opts$dateStart, dateEnd = opts$dateEnd, 
     tempDir = opts$tempDir, logOutLevel = opts$logOutLevel, overwrite = opts$overwrite)
