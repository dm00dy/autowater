#!/bin/bash

SCRIPT_PATH=/home/dmoody/espa2
DATA_PATH=/mnt/autowater/cdr

yyyymmdd=`date +%Y-%m-%d`

ndate=`date -d "$yyyymmdd -1 day" +%Y-%m-%d`


$SCRIPT_PATH/espa_order.sh -U ******@pointblue.org -P ***************** -p 44 -r 34 -d $ndate
sleep 1
$SCRIPT_PATH/espa_order.sh -U ******@pointblue.org -P ***************** -p 44 -r 33 -d $ndate
sleep 1
$SCRIPT_PATH/espa_order.sh -U ******@pointblue.org -P ***************** -p 43 -r 34 -d $ndate
sleep 1
$SCRIPT_PATH/espa_order.sh -U ******@pointblue.org -P ***************** -p 42 -r 35 -d $ndate
sleep 1


$SCRIPT_PATH/espa_download.sh

$SCRIPT_PATH/espa_dedupe.sh

$SCRIPT_PATH/espa_cdr.sh

