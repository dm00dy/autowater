h#!/bin/bash
# Master Autowater script
# Purpose: This script takes a date range and scene identifier (single path/row) and fully processes scenes
#			matching those criteria.  While processing multiple images is possible (from the same scene), 
#			it is best to do them individually, as an error for one image in one step will stop all further
#			automatic processing.  The scene already be downloaded.
#
# Example usage (see usage function below or run using -h flag for more info):
# bash autowater/scripts/run_autowater_all.sh -p44 -r33 -s "2016-01-01" -e "2016-01-16"
#
# Todos:
# 1. Keep an eye out for errors
# 2. Notification (email?) when errors happen
# 3. Set it to spin up a new instance and run there to save on processing costs
# 4. Get the date and scene from the download script to kick it all off
# 5. Mosaics
#
# One Shell to rule them all, one Shell to find them,
# One Shell to bring them all and in the darkness bind them
# 
# nelliott, July 2016

###########
## Setup ##
###########

# Define usage (for help)
programname=$0
function usage {
    echo ""
	echo "Usage: $programname [-h] -p path -r row -s dateStart [-e dateEnd = dateStart]"
    echo "	-h				display help"
	echo "	-p path			specify Landsat path id (string or integer)"
	echo "	-r row			specify Landsat row id (string or integer)"
	echo "	-s dateStart    specify start date; must be in an unabiguous date format (e.g., yyyy-mm-dd)"
	echo "	-e dateEnd		optional, specify end date, defaults to dateStart if not specified; must be in an unabiguous date format (e.g., yyyy-mm-dd)"
    exit 1
}

# Error handling
# Function that will take the first passed argument and concatenate it into an error message
# before exiting the script with code 1.
function error_exit {
	echo "[$(date '+%Y-%m-%d %H:%M:%S')] FATAL -- $1 Terminating autowater run." >&2
	exit 1
}

##################
## Check Inputs ##
##################

# Confirm what has been passed
echo ""
#echo Script Name: "$0"
#echo Total Number of Argument Passed: "$#"
#echo All Arguments are: "$*"

# Exit if too few parameters
if [ $# -lt 6 ]; then
	echo "Must specify path -p, row -r, and start date -s.  See proper usage below." >&2
	usage
fi

# Check/get passed arguments
while getopts "hp:r:s:e:" opt; do
  case $opt in
    h)
		echo "Showing help."
		usage
		;;
	p)
		path=$OPTARG
		echo "Path specified: $OPTARG"
		;;
	r)
		row=$OPTARG
		echo "Row specified: $OPTARG"	
		;;
	s)
		dateStart=$OPTARG
		echo "Start date specified: $OPTARG"		
		;;
	e)
		dateEnd=$OPTARG
		echo "End date specified: $OPTARG"		
		;;
    \?)
		echo "Invalid parameter specification.  See proper usage below." >&2
		usage
		;;
  esac
done

# Set end date to start date if not specified
if [ -z ${dateEnd+1} ]; then
	dateEnd=$dateStart
	echo "End date not specified...setting to start date."
fi

# Attempt to convert dates to desired format
dateStartFmt=$(date -d $dateStart "+%Y-%m-%d") || error_exit "Improperly formatted start date."
echo "Formatted start date: $dateStartFmt"
dateEndFmt=$(date -d $dateEnd "+%Y-%m-%d") || error_exit "Improperly formatted end date."
echo "Formatted end date: $dateEndFmt"

# Check path and row are numbers
case $path in
   (*[!0-9]*|'') error_exit "Path is not a number.";;
   (*)          ;;
esac

case $row in
   (*[!0-9]*|'') error_exit "Row is not a number.";;
   (*)           ;;
esac

# Build scene
scene="p${path}r${row}"
echo "Scene id: $scene"

######################
## Process Scene(s) ##
######################

# Change to autowater code dir
cd "/home/blue/sparklemotion/autowater/scripts/" || error_exit "Could not cd to script directory."

autowaterFunctions="standardizeLandsatRasters compileMetadata calculateNDWI calculateMNDWI stackLandsatRasters createCloudMasks 
						applyCloudMasks predictWaterRasters thresholdWaterRasters publishWaterRasters"

# Loop across autowater functions
for fxn in $autowaterFunctions; do
	
	# Call the autowater function
	echo "Now calling autowater function $fxn"
	Rscript run_autowater_function.r run $fxn --dateStart $dateStart --dateEnd $dateEnd --scene $scene || 
		error_exit "Non-recoverable error when running $fxn."

done