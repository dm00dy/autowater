# Functions for parsing the landsat metadata file
#
# nelliott, Nov 2015

# Requires conditions_base, input_testing, and file_manipulation
#
# Todo: define landsat metadata class

################
## Conditions ##
################

# Constructor function for a condition of (sub)class metadata_parsing_failure.  Signalled when a specified metadata
# file cannot be read and/or parsed.
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
metadata_parsing_failure <- function(text, condClass = "error", trigger = NA, ...) {
  
  msgBase <- "Specified metadata file cannot be loaded and/or parsed"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}

# Constructor function for a condition of (sub)class metadata_parsing_failure.  Signalled when a specified filename
# cannot be read and/or parsed.
filename_parsing_failure <- function(text, condClass = "error", trigger = NA, ...) {
  
  msgBase <- "Specified filename cannot be loaded and/or parsed"
  cond <- condition(condClass = condClass, msgBase = msgBase, msgDetail = text, trigger = trigger, ...)
  return(cond)
  
}


###############
## Functions ##
###############

# Function for finding landsat metadata files
#
# Args:
#   path: The directory in which to look for metadata files.
#   pattern (optional): The pattern to use to search for metadata files.  Defaults to "_MTL\\.txt$"
#   ... (optional): Additional arguments to list.files (e.g., recursive or full.names)
#
# Returns:
#   A vector of metadata files found.
listMetadataFiles <- function(path, searchPattern = "_MTL\\.txt$", ...) {
  
  # Report
  message(message_debug_enter())
  
  # Check input
  checkInput(argNames = c("path", "searchPattern"), 
             classes = c("character", "character"), 
             lengths = c(NA, 1), 
             fxnName = "getMetadataFiles")
  
  # Get files
  message(message_trace(paste0("Searching for metadata files in ", path, ".")))
  files <- sort(list.files(path, pattern = searchPattern, ...))
  message(message_trace(paste("Found", length(files), "matching metadata files.")))
  message(message_debug(paste("Files found:", paste(files, collapse = ", "))))
  
  # Check number
  if (length(files) == 0) {
    warning(file_missing("no matching source files found in directory.", "warning", sourceDir = path))
  }  
  
  # Report and return
  message(message_debug_exit())
  return(files)
  
}

# Function to read an original Landsat metadata file into memory as a data frame
#
# Args:
#   mdFileIn: The landsat metadata file to read in
#
# Returns:
#   A data frame with two columns, 'DataName' and 'DataValue', containing the name/value pairs from the original
#   text file.
readMetadata <- function(mdFile, condClass = "error") {
  
  # Report
  message(message_debug_enter())
  
  # Check input
  checkInput("mdFile", classes = "character", length = 1)
  checkFile(mdFile)
  
  # Read metadata file
  message(message_trace(paste0("Reading landsat metadata file ", mdFile, "...")))
  tryCatch({
    
    mdDf <- read.table(mdFile, header = FALSE, sep = "=", strip.white = TRUE, fill = TRUE, 
                       stringsAsFactors = FALSE)
    
    }, error = function(e) {
      
      raiseCondition(file_unreadable(paste0("cannot read landsat metadata file ", mdFile, "."), "error", e, 
                           metadataFile = mdFile))
      
    }
    
  )
  
  # Name columns
  names(mdDf) <- c("DataName", "DataValue")
  
  # Report and return
  message(message_debug_exit())
  return(mdDf)
  
}

# Function for parsing a landsat metadata data frame (generated by readOriginalMetadata)
# Args:
#   mdDf: The data frame of metadata to parse.
#   mdFileOut (optional): The csv file to write the metadata to if not NA.  Defaults to NA.
# Returns:
#   the parsed data frame
parseMetadata <- function(mdDf, mdFileOut = NA, condClass = "error") {
  
  # Report
  message(message_debug_enter())
  
  # Check input
  checkInput(argNames = c("mdDf", "mdFileOut"),
             classes = c("data.frame", NA),
             lengths = c(2, 1))
  
  message(message_trace("Parsing original landsat metadata..."))
  
  # Determine group rows
  groupRows <- which(mdDf$DataName == "GROUP")
  
  # Get the value of the most recent group entry for each row
  mdDf$Group <- vapply(1:nrow(mdDf), FUN = function(x) {mdDf$DataValue[groupRows[max(which(groupRows <= x))]]}, 
                       FUN.VALUE = character(1))
  
  # Remove group delimiters
  mdDf <- mdDf[mdDf$DataName != "GROUP" & mdDf$DataName != "END_GROUP" & mdDf$DataName != "END", ]
  
  # Add scene ID to each entry
  mdDf$Scene <- mdDf$DataValue[mdDf$DataName == "LANDSAT_SCENE_ID"]
  
  # Reorder
  mdDf <- mdDf[c("Scene", "Group", "DataName", "DataValue")]
  
  # Add processing info
  mdDf <- rbind(mdDf, c(mdDf$Scene[1], "POINT_BLUE_PROCESSING", "START_DATE", as.character(Sys.time())))
  
  # Export
  if (!is.na(mdFileOut)) writeFile("write.csv", "warning", x = mdDf, file = mdFileOut, row.names = FALSE)
  
  # Report and return
  message(message_debug_exit())
  return(mdDf)
  
}

# Keep full data for each scene in one file; flatten and keep only essential data 

# Flatten
#mdFlatDf <- as.data.frame(matrix(mdDf$DataValue, nrow = 1, byrow = TRUE))
#names(mdFlatDf) <- mdDf$DataName
flattenMetadata <- function(mdDf, mdFileOut = NA, condClass = "error") {
  
  # Report
  message(message_debug_enter())
  
  # Check inputs
  checkInput(argNames = c("mdDf", "mdFileOut", "condClass"),
             classes = c("data.frame", NA, "character"),
             length = c(NA, 1))
  
  # Get scene and establish base flattened df
  message(message_trace("Flattening metadata..."))
  tryCatch({
            mdMx <- matrix(c(mdDf[1, 1], mdDf$DataValue), nrow = 1)
            mdFlatDf <- data.frame(mdMx)
            names(mdFlatDf) <- c("Scene", mdDf$DataName)
            
          }, error = function(e) {
            
            raiseCondition(metadata_parsing_failure("could not flatten metadata passed to flattenMetadata."),
                           condClass, e, df = mdDf)
            
          })
  
  # Export
  if (!is.na(mdFileOut)) writeFile("write.csv", "warning", x = mdFlatDf, file = mdFileOut, row.names = FALSE)
    
  # Report and return
  message(message_debug_exit())
  return(mdFlatDf)
  
}

# Function to append metadata from one file into another
# 
# Args:
#   mdFrom: The metadata data frame OR file containing the data to append.
#   mdToFile: The metadata file to append the new data.
appendMetadata <- function(mdFrom, mdToFile, condClass = "error") {
  
  # Report
  message(message_debug_enter())
  
  # Check inputs
  checkInput(argNames = c("mdFrom", "mdToFile", "condClass"),
             classes = c(NA, "character", "character"),
             length = c(NA, 1, 1))
  
  # Check mdFrom
  if (is.character(mdFrom)) {
    checkFile(mdFrom)
    
    tryCatch(mdFromDf <- read.csv(mdFrom, stringsAsFactors = FALSE),
             error = function(e) {
               raiseCondition(file_unreadable(paste0("metadata file ", mdFrom, "."), condClass, e))
             })
    
  } else if (is.data.frame(mdFrom)) {
    mdFromDf <- mdFrom
    
  } else {
    raiseCondition(invalid_class("mdFrom must be either a dataframe or csv file.", condClass))
  }
  
  # If mdFileTo exists, read, append, and write
  if (file.exists(mdToFile)) {
    
    message(message_trace("Appending metadata..."))
    
      tryCatch(mdToDf <- read.csv(mdToFile, stringsAsFactors = FALSE),
              error = function(e) {
                raiseCondition(file_unreadable(paste0("metadata file ", mdToFile, "."), condClass, e))
              })
        
        # Rbind if names identical
        if (identical(names(mdToDf), names(mdFromDf))) {
          
          raiseCondition(message_debug("Identical names"))
          tryCatch(mdOutDf <- rbind(mdToDf, mdFromDf),
                    error = function(e) {
                      raiseCondition(metadata_parsing_failure(paste0("could not append metadata from mdFrom to ", 
                                                                     mdToFile, "."), condClass, e))
                   })
        
        # Else match column by column
        } else {
          
          raiseCondition(message_trace("Non-matching metadata column names. Appending row by row..."))
          
          # Use structure of file with more columns
          if (length(names(mdFromDf)) > length(names(mdToDf))) {
            raiseCondition(message_debug("Using structure of mdFromDf (more columns than mdToDf)"))
            mdOutDf <- mdFromDf[0, ]
          } else {
            raiseCondition(message_debug("Using structure of mdToDf (more columns than mdFromDf)"))
            mdOutDf <- mdToDf[0, ]
          }
          
          # Fill with NAs
          mdOutDf[1:(nrow(mdFromDf) + nrow(mdToDf)), ] <- NA
          
          # Loop across columns
          for (cl in names(mdOutDf)) {
            
            raiseCondition(message_debug(paste("Combining data for column", cl)))
            
            # If exists, get data from mdFromDf
            if (cl %in% names(mdFromDf)) {
              mdFromData <- mdFromDf[[cl]]
            } else {
              raiseCondition(message_trace(paste("Column", cl, "does not exist in mdFromDf.")))
              mdFromData <- rep(NA, nrow(mdFromDf))
            }
            
            # If exists, get data from mdToDf
            if (cl %in% names(mdToDf)) {
              mdToData <- mdToDf[[cl]]
            } else {
              raiseCondition(message_trace(paste("Column", cl, "does not exist in mdToDf.")))
              mdToData <- rep(NA, nrow(mdToDf))
            }
            
            # Deal with potential remaining factors
            mdData <- c(as.character(mdToData), as.character(mdFromData))
            raiseCondition(message_debug_vector("Combined metadata column data:", mdData))
            mdOutDf[[cl]] <- as.character(mdOutDf[[cl]])
            
            # Append
            mdOutDf[[cl]] <- mdData
            
          }
            
        }
        
        writeFile("write.csv", "error", x = mdOutDf, file = mdToFile, row.names = FALSE)
      
      #}, error = function(e) {
        
      #  raiseCondition(metadata_parsing_failure(paste0("could not append metadata from mdFrom to ", mdToFile, "."), 
      #                                          condClass, e))
        
      #})
    
  # Otherwise copy
  } else {
    
    raiseCondition(file_missing(paste0(mdToFile, ". Creating with data from this scene only."), "warning", file = mdToFile))
    mdOutDf <- mdFromDf
    #writeFile("write.csv", "error", x = mdOutDf, file = mdToFile, row.names = FALSE)
    write.csv(mdOutDf, mdToFile, row.names = FALSE)
    
  }
    
  # Report and return
  message(message_debug_exit())
  return(mdOutDf)
  
}

# Function that will read, parse, flatten, and append original landsat metadata (combination of above three)
processMetadata <- function(mdFileIn, mdFileFlat, mdFileAppend = NA, condClass = "error") {
  
  # Report
  message(message_debug_enter())
  
  # Check inputs
  checkInput(argNames = c("mdFileIn", "mdFileFlat", "mdFileAppend", "condClass"),
             classes = c("character", "character", NA, "character"),
             length = c(1, 1, 1, 1))
  
  # Check file existence
  checkFile(mdFileIn)

  # Process
  message(message_trace(paste0("Processing metadata file ", mdFileIn, "...")))
  mdDf <- readMetadata(mdFileIn, condClass = condClass)
  mdDf <- parseMetadata(mdDf, condClass = condClass)
  mdDf <- flattenMetadata(mdDf, mdFileOut = mdFileFlat, condClass = condClass)
  
  # Add base PB filename
  mdDf$Filename <- substr(createFilenamePB(parseFilenameUSGS(as.character(mdDf$Scene))), 1, 18)
  mdDf$ImageDate <- parseDate(substr(mdDf$Filename, 11, 18), "%Y%m%d")$DateOut
  
  locDf <- data.frame(Scene = c("p44r33", "p44r34", "p43r34", "p42r35"), 
                           Location = c("Sacramento", "Suisun", "San Joaquin", "Tulare"))
  mdDf$Location <- locDf$Location[match(substr(mdDf$Filename, 4, 9), locDf$Scene)]
  #print(mdDf)
  
  # Append if desired
  if (!is.na(mdFileAppend)) mdDf <- appendMetadata(mdDf, mdToFile = mdFileAppend, condClass = condClass)
  
  # Report and return
  message(message_trace(paste0("Metadata file ", mdFileIn, " processed successfully.")))
  message(message_debug_exit())
  return(mdDf)
  
}

# Function to return the metadata matching specific filename(s) from the combined metadata csv
#
# Arguments:
#   landsatFile:  the landsat filename(s) for which to get metadata
#   fnFormat:     the format of the landsat filename(s); must be "USGS" or "PointBlue"
#   mdFile:       the metadata file from which to extract metadata matching landsatFile
#   condClass:    type of condition to raise upon error; defaults to 'error'
#
# Returns:
#   data frame of metadata from mdFile matching parameters in fn
getLandsatMetadata <- function(landsatFile, fnFormat, mdFile, condClass = "error") {
  
  # Report
  message(message_debug_enter())
  
  # Check inputs
  checkInput(argNames = c("landsatFile", "fnFormat", "mdFile", "condClass"),
             classes = c("character", "character", "character", "character"),
             lengths = c(NA, 1, 1, 1))
  
  tryCatch({
  
      # Check metadata file existence
      checkFile(mdFile)
      
      # Read metadata file
      mdDf <- read.csv(mdFile)
      
      # Get relevant parameters from input filenames
      fnDf <- parseLandsatFilename(landsatFile, fnFormat)
      raiseCondition(message_debug_vector("Path:", fnDf$Path))
      raiseCondition(message_debug_vector("Row:", fnDf$Row))
      raiseCondition(message_debug_vector("Date:", fnDf$Date))
      
      # Filter metadata down to matching records
      mdDf <- mdDf[mdDf$WRS_PATH == fnDf$Path & mdDf$WRS_ROW == fnDf$Row & 
                      as.Date(mdDf$DATE_ACQUIRED) == as.Date(fnDf$Date), ]
      
      # Stop if missing records
      if(nrow(mdDf) != length(landsatFile)) {
        stop(paste0("Incorrect number of metadata matches returned: ", nrow(mdDf), " instead of ", 
                    length(landsatFile)))
      }
    
    }, error = function(e) {
      
      raiseCondition(metadata_parsing_failure("could not return metadata for specified file(s).",
                     condClass, e, df = mdDf))
      
    })
    
  # Report and return
  message(message_debug_exit())
  return(mdDf)
  
}


# Function to add processing info to the landsat master metadata table
#
#
appendProcessInfo <- function(landsatFile, fnFormat, mdFile, processName, processSuccess, processDatetime = format(Sys.time()), 
                           processNotes = "", additionalData = list(), write = FALSE, condClass = "error") {
  
  # Report
  message(message_debug_enter())
  
  # Check inputs
  checkInput(argNames = c("landsatFile", "fnFormat", "mdFile", "processName", "processSuccess", "processDatetime", "processNotes", 
                          "additionalData", "condClass"),
             classes = c("character", "character", "character", "character", "logical", "character", "character", "list", "character"),
             lengths = c(1, 1, 1, 1, 1, 1, 1, NA, 1))
  
  tryCatch({
    
    # Check metadata file existence
    checkFile(mdFile)
    
    # Read metadata file
    mdDf <- read.csv(mdFile)
    
    # Get relevant parameters from input filenames
    fnDf <- parseLandsatFilename(landsatFile, fnFormat)
    
    # Check that there are records
    if (nrow(mdDf[mdDf$WRS_PATH == fnDf$pth & mdDf$WRS_ROW == fnDf$Row & 
                   as.Date(mdDf$DATE_ACQUIRED) == as.Date(fnDf$Date), ]) == 0) {
      
      raiseCondition(metadata_parsing_failure("no metadata found matching specified file(s).", condClass, df = mdDf))
      
    } else {
      
      # Append processing info
      mdDf[mdDf$WRS_PATH == fnDf$pth & mdDf$WRS_ROW == fnDf$Row & 
             as.Date(mdDf$DATE_ACQUIRED) == as.Date(fnDf$Date), processName] <- processSuccess
      
      mdDf[mdDf$WRS_PATH == fnDf$pth & mdDf$WRS_ROW == fnDf$Row & 
             as.Date(mdDf$DATE_ACQUIRED) == as.Date(fnDf$Date), paste0(processName, "Datetime")] <- processDatetime
      
      mdDf[mdDf$WRS_PATH == fnDf$pth & mdDf$WRS_ROW == fnDf$Row & 
             as.Date(mdDf$DATE_ACQUIRED) == as.Date(fnDf$Date), paste0(processName, "Notes")] <- processNotes
      
      # Append additional data
      for (ad in 1:additionalData) {
        
        mdDf[mdDf$WRS_PATH == fnDf$pth & mdDf$WRS_ROW == fnDf$Row & 
               as.Date(mdDf$DATE_ACQUIRED) == as.Date(fnDf$Date), additionalData[ad]] <- additionalData[ad]
      
      }
      
      # Write if requested
      if (write == TRUE) {
        writeFile("write.csv", "error", x = mdDf, file = mdFile, row.names = FALSE)
      }
      
    }
    
  }, error = function(e) {
    
    raiseCondition(metadata_parsing_failure("could not add processing info for specified file(s).", condClass, e, df = mdDf))
    
  })
  
  # Report and return
  message(message_debug_exit())
  return(mdDf)
  
}

# Function to parse a landsat file name and return useful data.  
# 
# Args:
#   landsatFile: The name(s) of the file(s) to parse. May, but does not have to, include path.
#   fnFormat: The format the file(s) are in. Must be one of the recognized formats specified below, USGS or PointBlue.
#     All of the files must have the same formatting.
#
# Returns:
#   A data frame with satellite, procession, row, year, month, day, day of year, and version parsed out
parseLandsatFilename <- function(landsatFile, fnFormat, condClass = "error") {
  
  # Report
  message(message_debug_enter())
  
  # Check input
  checkInput(c("landsatFile", "fnFormat"), 
             classes = c("character", "character"), 
             length = c(NA, 1))
  
  if (!(fnFormat %in% c("USGS", "PointBlue"))) {
    
    stop(invalid_value(paste0("parseLandsatFilename does not understand the specified format(s) '", fnFormat, "."),
                       "error", invalidValues = fnFormat, allowedValues = supportedFormats))
  
  }
  
  # Parse
  tryCatch({
  
      # Grab filenames (if full file extension is passed)
      fn <- as.character(basename(landsatFile))
      
      # Parse
      if (fnFormat == "USGS") {
        fnDf <- parseFilenameUSGS(fn) 
      } else if (fnFormat == "PointBlue") {
        fnDf <- parseFilenamePB(fn)
      }
    
    }, error = function(e) {
    
      msg <- paste0(landsatFile, " using format ", fnFormat, ".")
      stop(filename_parsing_failure(msg, condClass, e, Files = landsatFile))
    
  })

  # Report and return
  message(message_debug_exit())
  return(fnDf)
  
}

# Function to parse a landsat file name, as formatted by USGS, and return useful data.  
#
# Example:
# LC80440332015224LGN00_B1
# SatPthRowYearDoyStnVn_Band
# Satellite, 3-digit path id, 3-digit row id, year, day of year, 3-letter station id, version, band designation
# 
# Args:
#   landsatFile: The name(s) of the file(s) to parse. May, but does not have to, include path.
#
# Returns:
#   A data frame with satellite, procession, row, year, month, day, day of year, and version parsed out
parseFilenameUSGS <- function(landsatFile) {
  
  # Report
  message(message_debug_enter())
  
  # Check input
  checkInput("landsatFile", "character", NA)
  
  # Load tools package (for file_ext)
  library(tools)
  
  # Grab file info (path, filename, extension)
  dirs <- dirname(landsatFile)
  fn <- basename(landsatFile)
  ext <- file_ext(landsatFile)
  extLength <- vapply(ext, FUN = nchar, FUN.VALUE = 1)
  
  # Satellite
  sat <- paste0(substr(fn, 1, 1), substr(fn, 3, 3))
  
  # Path and row
  pth <- as.character(as.numeric(substr(fn, 4, 6)))
  rw <- as.character(as.numeric(substr(fn, 7, 9)))
  scn <- paste0("p", pth, "r", rw)
  
  # Date
  dateStr <- substr(fn, 10, 16)
  dateDf <- parseDate(dateStr, "%Y%j")
  
  # Station
  stn <- as.character(substr(fn, 17, 19))
  
  # Version
  vsn <- as.character(substr(fn, 20, 21))
  
  # Band
  bnd <- as.character(substr(fn, 23, nchar(fn) - extLength - 1))
  bnd <- gsub("_|\\.", "", bnd)
  
  # Build data frame
  fnDf <- data.frame(FilePath = dirs, Filename = fn, Extension = ext, Satellite = sat, 
                     Scene = scn, Path = pth, Row = rw,
                     Date = dateDf$DateOut, Year = dateDf$Year, Month = dateDf$Month, Day = dateDf$Day, 
                     DayOfYear = dateDf$DayOfYear, Station = stn, VersionUSGS = vsn, Band = bnd, 
                     Type = "raw", VersionPB = NA)
  
  # Report and return
  message(message_debug_exit())
  return(fnDf)
  
}

# Function to parse a landsat file name, as formatted by Point Blue, and return useful data.  
#
# Example:
# L8_p44r33_20151203
# St_PthRow_YearMtDy
# Satellite_PathRow_YearMonthDay_type
# 
# Args:
#   landsatFile: The name(s) of the file(s) to parse. May, but does not have to, include path.
#
# Returns:
#   A data frame with satellite, procession, row, year, month, day, day of year, and version parsed out
parseFilenamePB <- function(landsatFile) {
  
  # Report
  message(message_debug_enter())
  
  # Check input
  checkInput("landsatFile", "character", NA)
  
  # Load tools package (for file_ext)
  library(tools)
  
  # Grab file info (path, filename, extension)
  dirs <- dirname(landsatFile)
  fn <- basename(landsatFile)
  ext <- file_ext(landsatFile)
  extLength <- vapply(ext, FUN = nchar, FUN.VALUE = 1)
  
  # Parse satellite
  sat <- paste0(substr(fn, 1, 2))
  
  # Parse path and row
  scn <- substr(fn, 4, 9)
  pth <- substr(scn, 2, 3)
  rw <- substr(scn, 5, 6)
  
  # Parse date
  dateStr <- substr(fn, 11, 18)
  dateDf <- parseDate(dateStr, "%Y%m%d")
  
  # Parse band
  if (substr(fn, 20, 20) == "B") {
    bnd <- as.character(substr(fn, 20, 22))
    bnd <- gsub("_|\\.", "", bnd)
  } else {
    bnd <- NA
  }
  
  # Parse type (processing step)
  if (nchar(fn) > 22) {
    re <- regexpr("_[a-zA-Z_]*\\_v.", fn)
    type <- substr(fn, re + 1, re + attr(re, "match.length") - 4)
  } else {
    type <- NA
  }
  
  # Parse PB version
  re <- regexpr("_v[0-9]{2}", fn)
  vsn <- substr(fn, re + 2, re + attr(re, "match.length") - 1)
  
  # Build data frame
  fnDf <- data.frame(FilePath = dirs, Filename = fn, Extension = ext, Satellite = sat, 
                     Scene = scn, Path = pth, Row = rw,
                     Date = dateDf$DateOut, Year = dateDf$Year, Month = dateDf$Month, Day = dateDf$Day, 
                     DayOfYear = dateDf$DayOfYear, Station = NA, VersionUSGS = NA, Band = bnd, 
                     Type = type, VersionPB = vsn)
  
  # Report and return
  message(message_debug_exit())
  return(fnDf)
  
}

#parseFilenamePB("C:/data/L5_p42r35_20000601_B3.tif")

# Function to create a PointBlue-formatted filename given a satellite, scene, date, band, and processing step (type)
#
# Args:
#   fnDf:
#   newType (optional): A new penultimate ending for the filename, e.g., "water_coverage". Defaults to NA, which will
#     keep the old type (set in fnDf).  Use empty string "" to remove the old type from the new filename without 
#     replacing with new.
#
# Returns:
#   A character vector of filenames the same length as fnDf
createFilenamePB <- function(fnDf, newType = NA, newVersion = NA, newExt = NA, 
                             dateFormatIn = "%Y-%m-%d", dateFormatOut = "%Y%m%d", condClass = "error") {
  
  # Report
  message(message_debug_enter())
  
  # Check input
  checkInput(c("fnDf", "newType", "newVersion", "newExt", "dateFormatIn", "dateFormatOut", "condClass"),
             c("data.frame", NA, NA, NA, "character", "character", "character"),
             c(NA, NA, NA, NA, NA, NA, 1))
  
  # Check lengths
  if (length(newType) != 1 & length(newType) != nrow(fnDf)) {
    
    newType <- newType[1]
    warning(invalid_length("newType must be either of length 1 or the same length as rows in fnDf. Only using
                           first element.", "warning", invalidVector = newType, matchingVector = nrow(fnDf)))
  
  }
  
  # Remove bad characters from newType and version (so as to not mess up filename)
  typCln <- gsub("[^[:alnum:]]", "", newType)
  if (!identical(typCln, newType) & !is.na(newType)) {
    
    message(message_debug_vector("Cleaned types:", typCln))
    warning(invalid_value(paste("non-alphanumeric characters present in newType. Invalid characters removed",
                              "from generated filename."),"warning", invalidValue = newType))
  
  }
  
  vsnCln <- gsub("[^[:alnum:]]", "", newVersion)
  if (!identical(vsnCln, newVersion) & !is.na(newVersion)) {
    
    message(message_debug_vector("Cleaned versions:", vsnCln))
    warning(invalid_value(paste("non-alphanumeric characters present in newVersion. Invalid characters removed",
                                "from generated filename."),"warning", invalidValue = newVersion))
    
  }
  
  # Build filenames
  tryCatch({
    
    # Format date
    dateFmt <- as.Date(fnDf$Date, format = dateFormatIn)
    dateStr <- format(dateFmt, format = dateFormatOut)
    
    # Create band string
    bndStr <- ifelse(is.na(as.character(fnDf$Band)) | nchar(as.character(fnDf$Band)) == 0, "", paste0("_", fnDf$Band))
    bndStr <- gsub("srband", "B", bndStr)
    bndStr <- gsub("band", "B", bndStr)
    message(message_debug_vector("Band vector for filename(s):", fnDf$Band))
    message(message_debug_vector("Band string vector for filename(s):", bndStr))
    
    # Create type string (last part of filename before extension)
    if (is.na(newType)) {
      typStr <- paste0("_", fnDf$Type)
    } else if (nchar(typCln) == 0) {
      typStr <- ""
    } else {
      typStr <- paste0("_", typCln)
    }
    message(message_debug_vector("Type string vector for filename(s):", typStr))
    
    # Create new version string
    if (is.na(newVersion)) {
      vsnStr <- paste0("_v", fnDf$VersionPB)
    } else if (nchar(vsnCln) == 0) {
      vsnStr <- ""
    } else {
      vsnStr <- paste0("_v", vsnCln)
    }
    
    # Create extension string
    if (is.na(newExt)) {
      extStr <- fnDf$Extension
    } else {
      extStr <- newExt
    }
    
    if (substr(extStr, 1, 1) != ".") {
      extStr <- paste0(".", extStr)
    }
    extStr <- tolower(extStr)
    message(message_debug_vector("Extension string vector for filename(s):", extStr))
    
    # Create new filenames
    fn <- paste0(fnDf$Satellite, "_", fnDf$Scene, "_", dateStr, bndStr, typStr, vsnStr, extStr)
    message(message_debug_vector("Created filename(s):", fn))
    
  },
  
    error = function(e) {
      
      raiseCondition(filename_parsing_failure("cannot create filenames from passed data.", condClass, 
                                              e, badData = fnDf))
     
  })
  
  # Report and return
  message(message_debug_exit())
  return(fn)
  
}

# Function to parse a date, returning year, month, day, and day of year. Calls as.Date and format.
# 
# Args:
#   dateString: The date string (as a character vector) to be parsed.
#   dateFormatIn: The format of the date string dateStr using a character string as.Date and format can understand
#     as a proper date format. Must be one or the same length as dateString.
#   dateFormatOut (optional): The format of the returned date string; defaults to "%Y%m%d" (YYYYMMDD).
#
# Returns:
#   A data frame with the input date, formated date, year, month, day, and day of year parsed out
parseDate <- function(dateString, dateFormatIn, dateFormatOut = "%Y-%m-%d") {
  
  # Report
  message(message_debug_enter())
  
  # Check input
  checkInput(c("dateString", "dateFormatIn", "dateFormatOut"), 
             c("character", "character", "character"), 
             c(NA, NA, 1))
  
  # Check length of dateFormat
  if (length(dateFormatIn) != 1 & length(dateFormatIn) != length(dateString)) {
    
    dateFormat <- dateFormat[1]
    warning(invalid_length("dateFormat must be either of length 1 or the same length as dateString. Only using
                           first element.", "warning", invalidVector = dateFormatIn, matchingVector = dateString))
    
  }
  
  # Parse date
  dateFmt <- as.Date(dateString, format = dateFormatIn)
  yr <- format(dateFmt, format = "%Y")
  mth <- format(dateFmt, format = "%m")
  dy <- format(dateFmt, format = "%d")
  doy <- format(dateFmt, format = "%j")
  dateStrOut <- format(dateFmt, format = dateFormatOut)
  message(message_debug(paste("Input date string:", dateString)))
  message(message_debug_vector("Parsed date string, year, month, day, and day of year:", 
                               vector = c(dateStrOut, yr, mth, dy, doy)))
  
  # Build data frame
  dateDf <- data.frame(DateIn = dateString, DateOut = dateStrOut, Year = yr, Month = mth, Day = dy, DayOfYear = doy)
  
  # Report and return
  message(message_debug_exit())
  return(dateDf)
  
}

# Function to parse a scene string for path and row. Must be in format p44r33 or 044033.
#
# Args:
#
# Check scene formatting
parseScene <- function(sceneString, altSceneValue = NA) {
  
  # Report
  message(message_debug_enter())
  scenes <- list()
  
  if (all(substr(sceneString, 1, 1) == "p") & all(substr(sceneString, 4, 4) == "r")) {
    
    message(message_debug("Scene format: p44r34"))
    scenes[[1]] <- sceneString
    scenes[[2]] <- paste0("0", substr(sceneString, 2, 3), "0", substr(sceneString, 5, 6))
    
  } else if (all(grepl("[0-9]{6}", sceneString))) {
    
    message(message_debug("Scene format: 044034"))
    if (!(all(substr(sceneString, 1, 1) == 0 & substr(sceneString, 4, 4) == 0))) {
      raiseCondition(invalid_value(paste("scenes with paths or rows in the hundreds may not be returned if files are",
                                         "stored in format p44r34."), "warning", invalidValue = sceneString))
    }
    scenes[[1]] <- paste0("p", substr(sceneString, 2, 3), "r", substr(sceneString, 5, 6))
    scenes[[2]] <- sceneString
    
  } else if (length(sceneString) == 1 & !is.na(altSceneValue) & sceneString == altSceneValue) {
    
    scenes[[1]] <- sceneString
    scenes[[2]] <- sceneString
    
  } else {
    
    raiseCondition(invalid_value("scene format must either be p44r34 or 044034.", "error", invalidValue = sceneString))
    
  }
  
  # Report and return
  message(message_debug_exit())
  return(scenes)

}
  
# Function to get a list of files in a directory within a passed date range
#
# Args:
#   path: The directory in which to search for matching files.
#   sceneChars (optional): A vector with the starting and ending characters of the scene for files in path. Defaults to
#     c(4, 9).
#   scenes (optional): A character vector of the scenes to include, using format "p44r34" or "044034". Defaults to
#     the four central valley scenes (c("p42r35", "p43r34", "p44r33", "p44r34")).
#   dateChars (optional): A vector with the starting and ending characters of the date for files in path.  Defaults 
#     to c(11, 18) which is the point blue landsat format.
#   dateFormat (optional): The format of the date specified by dateChars above.  Defaults to the PB format "%Y%m%d".
#   dateStart (optional): The start date of files to look for (inclusive).  Must be in format yyyy-mm-dd.  
#     Defaults to today.
#   dateEnd (optional): The end date of files to look for (inclusive).  Must be in format yyyy-mm-dd.
#     Defaults to today.
#   searchPattern (optional): The search pattern to use when looking for valid files in path.  Defaults to 
#     tif files (".*tif$").
#   ... (optional): Additional arguments to pass to list.files (e.g., full.names = TRUE, recursive = FALSE)
listFilesBySceneDate <- function(path, sceneChars = c(4, 9), scenes = c("p42r35", "p43r34", "p44r33", "p44r34"),
                                  dateChars = c(11, 18), dateFormat = "%Y%m%d",  
                                  dateStart = format(Sys.time(), "%Y-%m-%d"), dateEnd = format(Sys.time(), "%Y-%m-%d"), 
                                  searchPattern = ".*tif$", altSceneValue = NA, ...) {
  
  # Report
  message(message_debug_enter())
  
  # Check input
  checkInput(c("path", "sceneChars", "scenes", "dateChars", "dateFormat", "searchPattern", "dateStart", "dateEnd"), 
             c("character", "numeric", "character", "numeric", "character", "character", "character", "character"), 
             c(1, 2, NA, 2, 1, 1, 1, 1))
  
  checkDir(path, "warning")
  
  # Check start date
  dateStart <- tryCatch(format(as.Date(dateStart), format = "%Y-%m-%d"), error = function(e) {
    raiseCondition(invalid_value(paste0("Given start date ", dateStart, " cannot be converted into a valid date. Exiting."),
                                 "error", e))
  })
  
  # Check end date
  dateEnd <- tryCatch(format(as.Date(dateEnd), format = "%Y-%m-%d"), error = function(e) {
    raiseCondition(invalid_value(paste0("Given end date ", dateEnd, " cannot be converted into a valid date. Exiting."),
                                 "error", e))
  })
  
  # Check date difference
  if (dateEnd < dateStart) {
    dateEnd <- dateStart
    raiseCondition(invalid_value(paste0("Given end date ", dateEnd, " is before given start date ", dateStart, ". ",
                                        "Changing dateEnd to dateStart.", "warning")))
  }
  
  # Get files matching searchPattern in path
  message(message_trace("Searching for matching files..."))
  files <- list.files(path, pattern = searchPattern, ...)
  message(message_debug_vector("File(s) in path matching searchPattern:", files))
  
  # Parse scene formatting
  scenes <- parseScene(scenes, altSceneValue)
  
  # Extract scenes
  message(message_debug_vector("Base filenames:", basename(files)))
  scenesStr <- substr(basename(files), sceneChars[1], sceneChars[2])
  message(message_debug_vector("Scene string(s):", scenesStr))
  
  # Extract dates
  datesStr <- substr(basename(files), dateChars[1], dateChars[2])
  message(message_debug_vector("Date string(s):", datesStr))
  dates <- format(as.Date(datesStr, dateFormat), "%Y-%m-%d")
  message(message_debug_vector("Date(s):", dates))
  
  # Match
  matches <- na.omit(files[(scenesStr %in% scenes[[1]] | scenesStr %in% scenes[[2]]) & 
                             dates >= dateStart & dates <= dateEnd])
  message(message_debug_vector("Match(es):", matches))
  message(message_trace(paste("Found", length(matches), "matching file(s).")))
  
  # Warn if no matches
  if (length(matches) == 0) {
    warning(file_missing("no files matching searchPattern and with correct dates found in specified path.", 
                         "warning", sourceDir = path, dateChars = dateChars, dateFormat = dateFormat, 
                         searchPattern = searchPattern, dateStart = dateStart, dateEnd = dateEnd))
  }
  
  # Report and return
  message(message_debug_exit())
  return(matches)
  
}