#!/bin/bash

# start our search on the first aquisition date, or adjust to start later

inceptdate=20130415

# end the search on the current date, or adjust to end earlier
currdate=`date +%Y%m%d`

let "operatingdays = $(( ($(date --date=$currdate +%s) - $(date --date=$inceptdate +%s) )/(60*60*24) ))";
let "cycles =  ($operatingdays/16+1)";


instrument='LC8'
BUCKET_PATH=gs://pointblue-autowater-cdr



paths[0]=44
rows[0]=33
offsetdays[0]=1

paths[1]=44
rows[1]=34
offsetdays[1]=1

paths[2]=42
rows[2]=35
offsetdays[2]=3

paths[3]=43
rows[3]=34
offsetdays[3]=10


for (( i=0; i<$cycles; i++ ))
do
	
	for (( j=0; j<=3; j++ ))
	do

		path=${paths[$j]}
		row=${rows[$j]}
		offsetday=${offsetdays[$j]}
		
		let aqday="$i*16"
		sceneday=$(($aqday + $offsetday))  
		
		# zero-pad the row and path to 3 digits, but force conversion to decimal first (otherwise zero-padded arguments evaluate as octal).
		printf -v path "%03d" $((10#$path))
		printf -v row "%03d" $((10#$row))
		 
		scenedate=`date -d "$inceptdate +$sceneday day" +%Y-%m-%d`
		year=`date -d $scenedate '+%Y'`
		jdate=`date -d $scenedate '+%j'`


		SCENE_ID=${instrument}${path}${row}${year}${jdate}
		
		# remove any partially downloaded scenes.
		rm /tmp/${SCENE_ID}*.part
		
		for file in /tmp/${SCENE_ID}*; do mv "$file" /tmp/${SCENE_ID}.tar.gz; done
		
		
	done

done

