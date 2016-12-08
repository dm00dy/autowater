#!/bin/bash


# use gcsfuse to mount the cloud bucket locally.
# https://cloud.google.com/storage/docs/gcs-fuse
# $ mkdir /path/to/mount
# git clone https://github.com/USGS-EROS/espa-bulk-downloader.git bulk-downloader

# mkdir /mnt/data
# mkdir /mnt/data/misc
# gcsfuse pointblue-autowater-misc /mnt/data/misc

# python download_espa_order.py --email dmoody@pointblue.org --username dmoody@pointblue.org --password DM7699pw --order ALL -d /tmp



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
		
		
		# echo ${SCENE_ID} ${operatingdays}
		
		if gsutil -q stat ${BUCKET_PATH}/${SCENE_ID}/${SCENE_ID}*.tar.gz
		then
			echo "Scene ${SCENE_ID} ($scenedate) exists in Google Cloud Storage."
		elif [ -f /tmp/${SCENE_ID}*.tar.gz ] 
		then
			
			echo "Scene ${SCENE_ID} ($scenedate) exists locally.  Ready to upload to Google Cloud Storage."
			gsutil cp /tmp/${SCENE_ID}*.tar.gz gs://pointblue-autowater-cdr/${SCENE_ID}/${SCENE_ID}.tar.gz
			# rm /tmp/${SCENE_ID}*.tar.gz
			
			printf -v path "%02d" $((10#$path))
			printf -v row "%02d" $((10#$row))
		
			# python /home/blue/sparklemotion/autowater/scripts/run_autowater_all.py ${path} ${row} -s $scenedate -e $scenedate
			echo "python /home/blue/sparklemotion/autowater/scripts/run_autowater_all.py ${path} ${row} -s $scenedate -e $scenedate"
			
	
		else
			echo "Scene ${SCENE_ID} ($scenedate) does not exist."
		fi
		
		

	done

done

# python download_espa_order.py --email dmoody@pointblue.org --username **************** --password **************** --order ALL -d /tmp

