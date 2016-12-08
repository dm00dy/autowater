#!/bin/bash


# start our search on the first aquisition date, or adjust to start later

inceptdate=20130415

# end the search on the current date, or adjust to end earlier
currdate=`date +%Y%m%d`

let "operatingdays = $(( ($(date --date=$currdate +%s) - $(date --date=$inceptdate +%s) )/(60*60*24) ))";
let "cycles =  ($operatingdays/16+1)";


instrument='LC8'
BUCKET_PATH=gs://pointblue-autowater-cdr


offsetdays[0]=1
offsetdays[1]=10

offsetends[0]=9
offsetends[1]=8



for (( i=0; i<=$cycles; i++ ))
do
	

	num_cycle_scenes=0
	# cycledate=`date -d "$inceptdate +$aqday day" +%Y-%m-%d`
		
	for (( j=0; j<=1; j++ ))
	do

		offsetday=${offsetdays[$j]}
		offsetend=${offsetends[$j]}
		
		let aqday="$i*16"

		scenedate=`date -d "$inceptdate +$(($aqday + $offsetday)) day" +%Y%m%d`

		scenedate_2=`date -d "$inceptdate +$(($aqday + $offsetday + $offsetend)) day" +%Y%m%d`
		
		

		SCENE_ID=L8_valley_${scenedate}to${scenedate_2}_water
		
		echo ${SCENE_ID} ${operatingdays}
		
		# example: gs://pointblue-autowater-pub/mosaics/L8_valley_20130416to20130425_water.tar.gz
		
		
		if /home/doug/landsat8/google-cloud-sdk/bin/gsutil -q stat gs://pointblue-autowater-pub/mosaics/${SCENE_ID}.tar.gz
		then
			
			((num_cycle_scenes++))
			
			echo "Scene ${SCENE_ID} ($scenedate) exists."
			
			
			/home/doug/landsat8/google-cloud-sdk/bin/gsutil cp gs://pointblue-autowater-pub/mosaics/${SCENE_ID}.tar.gz /mnt/data/dmoody/outfiles/mosaics

			cd /mnt/data/dmoody/outfiles/mosaics

			gunzip /mnt/data/dmoody/outfiles/mosaics/${SCENE_ID}.tar.gz
			tar -xvf ${SCENE_ID}.tar
			
			gdalwarp -t_srs EPSG:3310 -dstnodata -9999 -co COMPRESS=LZW ${SCENE_ID}.tif ${SCENE_ID}.3310.tif
			
			
			gdaldem color-relief -alpha -of PNG ${SCENE_ID}.3310.tif /home/doug/espa2/autowater-alpha.ramp.txt ${SCENE_ID}.3310.png

			gdaldem color-relief ${SCENE_ID}.3310.tif /home/doug/espa2/autowater-alpha.ramp.txt ${SCENE_ID}.rgb.tif
			
			gdal_translate -of KMLSUPEROVERLAY ${SCENE_ID}.rgb.tif ${SCENE_ID}.kmz
			
			# the demo version used Z14.  We could probably go to Z15 at the cost of rendering time.	
			/home/doug/gdal2tiles.py -z 1-14 ${SCENE_ID}.3310.png tiles/${SCENE_ID}
		
			rm -rf /mnt/data/tiles/water/${SCENE_ID}
			
			mv /mnt/data/dmoody/outfiles/mosaics/tiles/${SCENE_ID} /mnt/data/tiles/water/
			
			mv ${SCENE_ID}.3310.tif /mnt/data/tif/water/
			
			mv ${SCENE_ID}.3310.png* /mnt/data/png/water/
			
			/home/doug/landsat8/google-cloud-sdk/bin/gsutil cp ${SCENE_ID}.kmz gs://pointblue-autowater-pub/mosaics/
			

		
			
			
			fi
			
			

		
		

	done
	
	echo "${i} ($num_cycle_scenes)."
		
done


