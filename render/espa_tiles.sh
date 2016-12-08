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

for (( i=0; i<=$cycles; i++ ))
do
	

	num_cycle_scenes=0
	# cycledate=`date -d "$inceptdate +$aqday day" +%Y-%m-%d`
		
	for (( j=0; j<=3; j++ ))
	do

		path=${paths[$j]}
		row=${rows[$j]}
		offsetday=${offsetdays[$j]}
		
		let aqday="$i*16"
		sceneday=$(($aqday + $offsetday))  
		
		# zero-pad the path to 3 digits, but force conversion to decimal first (otherwise zero-padded arguments evaluate as octal).
		printf -v path "%03d" $((10#$path))
		# zero-pad the row  to 3 digits, but force conversion to decimal first (otherwise zero-padded arguments evaluate as octal).
		printf -v row "%03d" $((10#$row))
		 
		scenedate=`date -d "$inceptdate +$sceneday day" +%Y-%m-%d`
		year=`date -d $scenedate '+%Y'`
		jdate=`date -d $scenedate '+%j'`


		SCENE_ID=${instrument}${path}${row}${year}${jdate}
		

		
		BUCKET_PATH_PUBLIC=gs://pointblue-autowater-pub
		scenedate=`date -d "$inceptdate +$sceneday day" +%Y%m%d`
		# zero-pad the path to 3 digits, but force conversion to decimal first (otherwise zero-padded arguments evaluate as octal).
		printf -v path "%02d" $((10#$path))
		# zero-pad the row  to 3 digits, but force conversion to decimal first (otherwise zero-padded arguments evaluate as octal).
		printf -v row "%02d" $((10#$row))
		
		SCENE_ID=L8_p${path}r${row}_${scenedate}_water
		
		echo ${SCENE_ID} ${operatingdays}
		
		# example: gs://pointblue-autowater-pub/single_scenes/L8_p42r35_20130418_water.tar.gz
		
		
		if /home/doug/landsat8/google-cloud-sdk/bin/gsutil -q stat gs://pointblue-autowater-pub/single_scenes/${SCENE_ID}.tar.gz
		then
			
			((num_cycle_scenes++))
			
			echo "Scene ${SCENE_ID} ($scenedate) exists."
			

			
				
				/home/doug/landsat8/google-cloud-sdk/bin/gsutil cp gs://pointblue-autowater-pub/single_scenes/${SCENE_ID}.tar.gz /mnt/data/dmoody/outfiles

				cd /mnt/data/dmoody/outfiles

				gunzip /mnt/data/dmoody/outfiles/${SCENE_ID}.tar.gz
				tar -xvf ${SCENE_ID}.tar
				
				gdalwarp -t_srs EPSG:3310 -dstnodata -9999 -co COMPRESS=LZW L8_p${path}r${row}_${scenedate}_water.tif L8_p${path}r${row}_${scenedate}_water.3310.tif
				
				
				gdaldem color-relief -alpha -of PNG L8_p${path}r${row}_${scenedate}_water.3310.tif /home/doug/espa2/autowater-alpha.ramp.txt L8_p${path}r${row}_${scenedate}_water.3310.png

				gdaldem color-relief L8_p${path}r${row}_${scenedate}_water.3310.tif /home/doug/espa2/autowater-alpha.ramp.txt L8_p${path}r${row}_${scenedate}_water.rgb.tif
				
				gdal_translate -of KMLSUPEROVERLAY L8_p${path}r${row}_${scenedate}_water.rgb.tif L8_p${path}r${row}_${scenedate}_water.kmz
				
				# the demo version used Z14.  We could probably go to Z15 at the cost of rendering time.	
				/home/doug/gdal2tiles.py -z 1-14 L8_p${path}r${row}_${scenedate}_water.3310.png tiles/L8_p${path}r${row}_${scenedate}_water
			
				rm -rf /mnt/data/tiles/water/L8_p${path}r${row}_${scenedate}_water
				
				mv /mnt/data/dmoody/outfiles/tiles/L8_p${path}r${row}_${scenedate}_water /mnt/data/tiles/water/
				
				mv L8_p${path}r${row}_${scenedate}_water.3310.tif /mnt/data/tif/water/
				
				mv L8_p${path}r${row}_${scenedate}_water.3310.png* /mnt/data/png/water/
				
				/home/doug/landsat8/google-cloud-sdk/bin/gsutil cp L8_p${path}r${row}_${scenedate}_water.kmz gs://pointblue-autowater-pub/single_scenes/
				
				
			
			
			
			fi
			
			
		
		

	done
	
	echo "${i} ($num_cycle_scenes)."
		
done



