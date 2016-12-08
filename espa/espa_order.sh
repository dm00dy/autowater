#!/bin/bash

SCRIPT_PATH=`dirname ${BASH_SOURCE[0]}`

source $SCRIPT_PATH/options.sh

instrument='LC8'
station='LGN'
version='00'


# yyyymmdd=`date +%Y-%m-%d`


ndate=`date -d "$yyyymmdd" +%Y-%m-%d`



year=`date -d $ndate '+%Y'`
jdate=`date -d $ndate '+%j'`


SCENE_FILE=${instrument}${path}${row}${year}${jdate}${station}${version}



curlstring="{\"olitirs8\": {\"inputs\": [\"${SCENE_FILE}\"], \"products\": [\"sr\", \"l1\", \"source_metadata\"] }, \"format\": \"gtiff\"}"


curl -k --user ${username}:${password} -d "${curlstring}" https://espa.cr.usgs.gov/api/v0/order

# working API request:
# curl -k --user ***************:*************** -d '{"olitirs8": {"inputs": ["LC80430342016252LGN00"], "products": ["sr", "l1", "source_metadata"] }, "format": "gtiff"}' https://espa.cr.usgs.gov/api/v0/order
