# Master Autowater script
# Purpose: This script takes a date range and scene identifier (single path/row) and fully processes scenes
#			matching those criteria.  While processing multiple images is possible (from the same scene), 
#			it is best to do them individually, as an error for one image in one step will stop all further
#			automatic processing.  The scene must already be downloaded.
#
# Example usage (see usage function below or run using -h flag for more info):
# cd sparklemotion/autowater/scripts
# python run_autowater_all.py 43 34 -s "2016-01-01" -e "2016-01-16"
# OR
# python /home/blue/sparklemotion/autowater/scripts/run_autowater_all.py 43 34 -s "2016-01-01" -e "2016-01-16"
#
# Todos:
# 1. Keep an eye out for errors
# 2. Prettify messages
# 3. Add mosaics
# 4. Change to fully process one date, then move to next when processing multiple dates?  Pros/cons each way.
# 5. Add error options: die vs next
# 
# nelliott, Oct 2016

# Define usage (for help)
"""Function to run autowater R functions from command line.

Usage: 
  run_autowater_all <path> <row> [--dateStart=<ds> --dateEnd=<de> -v]
  run_autowater_all --version
  run_autowater_all -h | --help

Arguments:
  <path>                Landsat path id to process (string or integer)
  <row>                 Landsat row id to process (string or integer)
  
Options:
  -h, --help            Show this screen.
  --version             Show the version.
  -s, --dateStart=<ds>  First date of files to process, using format yyyy-mm-dd; defaults to today.
  -e, --dateEnd=<de>    First date of files to process, using format yyyy-mm-dd; defaults to today.
  -v                    Run verbosely.


"""

# Import modules
from docopt import docopt
import datetime, subprocess, sys

# Function to validate dates
def validateDate(dateText, varName = ""):
  try:
    datetime.datetime.strptime(dateText, '%Y-%m-%d')
  except ValueError:
    raise ValueError("Incorrect date format: " + varName + " should be formatted as YYYY-MM-DD.")
    
# Function to validate path and row
def validateNums(x, varName = ""):
  try:
      return int(x)
  except ValueError:
      raise ValueError("Incorrect path/row format: " + varName + " should be an integer or convertable to integer.")

# Email notification functions

# Import modules
import sys
import smtplib
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText

# Creates and returns a multipart MIME with the specified sender, reciever, subject, and body
def emailBuild(sender = "nelliott@pointblue.org", reciever = "nelliott@pointblue.org", 
subject = "Automated Water Report", body = "Automated water report"):
  
  # Create message (multipart MIME)
  msg = MIMEMultipart()
  
  # Add attributes
  msg["Subject"] = subject
  msg["From"] = sender
  msg["To"] = reciever
  msg.preamble = subject
  
  # Attach text
  msg.attach(MIMEText(body, "plain"))
  
  # Return
  return(msg)

# More specific email build
def emailBuildError(subject = "Error in Automated Water Processing", 
body = "An error was encountered when running run_autowater_all.py. Please check server."):
  
  # Call emailBuild
  msg = emailBuild(subject = subject, body = body)
  return(msg)


# Function to send an email from a MIME object and specified server, username, and password
def emailSend(msg, smtpServer = "smtp.gmail.com:587", 
username = "nathan.k.elliott@gmail.com", password = "ttlipbbfrhsfffxs"):
  
  try:
    server = smtplib.SMTP(smtpServer)
    server.ehlo()
    server.starttls()
    server.login(username, password)
    server.sendmail(msg["From"], msg["To"], msg.as_string())
  except:
    print "Error sending email: ", sys.exc_info()[0]
  else:
    print "Notification email sent."
  finally:
    server.quit()



# Run
if __name__ == '__main__':
    
    # Get arguments
    args = docopt(__doc__, version="Autwater Master Script 0.9")
    pth = args["<path>"]
    rw = args["<row>"]
    dateStart = args["--dateStart"]
    dateEnd = args["--dateEnd"]
    verbose = args["-v"]
    
    # Report if verbose
    if verbose:
      print args
      print "Path:", pth
      print "Row:", rw
    
    # Check path and row are numbers
    validateNums(pth, "path")
    validateNums(rw, "row")
    
    # Check dates, defaulting to today if missing
    if dateStart is None:
      print "No start date specified so defaulting to today."
      dateStart = datetime.date.today().strftime("%Y-%m-%d")
    else:
      validateDate(dateStart, "dateStart")
    
    if dateEnd is None:
      print "No end date specified so defaulting to today."
      dateEnd = datetime.date.today().strftime("%Y-%m-%d")
    else:
      validateDate(dateEnd, "dateEnd")
    
    # Check that dateEnd >= dateStart
    if dateEnd < dateStart:
      raise ValueError("Start date must not be after end date!")
    
    if verbose:
      print "Start date:", dateStart
      print "End date:", dateEnd
    
    # Build scene
    scene = "p" + pth + "r" + rw
    
    if verbose:
      print "Scene:", scene
    
    # Process scenes
    autowaterFunctions = ["standardizeLandsatRasters", "compileMetadata", "calculateNDWI", "calculateMNDWI", "stackLandsatRasters", 
                  "createCloudMasks", "bufferCloudMasks", "applyCloudMasks", "predictWaterRasters", "thresholdWaterRasters", 
                  "validateAndPublishWaterRasters"]
                  
    for af in autowaterFunctions:
      
      print "Calling autowater function", af 
      
      # Build call
      cmd = "Rscript /home/blue/sparklemotion/autowater/scripts/run_autowater_function.r run " + af
      cmd = cmd + " --dateStart " + dateStart + " --dateEnd " + dateEnd + " --scene " + scene
      if verbose:
        print "Command line call:", cmd
      
      # Make call
      p = subprocess.Popen(cmd, shell = True, stdin = subprocess.PIPE, stdout = subprocess.PIPE, stderr = subprocess.PIPE,
          bufsize=-1)
      
      # Handle output
      logMsg = ""
      err = False
      while True:
        out = p.stderr.read(1)
        
        # Catch/flush text
        if out != "":
          sys.stdout.write(out)
          sys.stdout.flush()
          logMsg = logMsg + out
        
        # End when done, capturing return code
        if out == "" and p.poll() != None:
          rc = p.poll()
          break
        
      # Handle errors
      if rc != 0:
        
        errMsg = "Non-recoverable error when running script " + af + ". Terminating processing.\n\n" + \
                    "Command line call:\n\n" + cmd + "\n\nFull log:\n\n" + logMsg
        #print errMsg
        
        # Email admin
        msg = emailBuildError(body = errMsg)
        emailSend(msg)
        
        # End
        break

