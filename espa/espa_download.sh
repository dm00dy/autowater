#!/bin/bash

#
# PREREQUISITES:
# use gcsfuse to mount the cloud bucket locally.
# https://cloud.google.com/storage/docs/gcs-fuse
# Create the mountpoint.
# mkdir /mnt/data
# mkdir /mnt/data/misc
# 
# git clone https://github.com/USGS-EROS/espa-bulk-downloader.git bulk-downloader
#

# mount the bucket in the local filesystem.
# gcsfuse pointblue-autowater-misc /mnt/data/misc

# download the ESPA order queue into the fuse bucket.
# python /home/dmoody/espa2/bulk-downloader/download_espa_order.py --email dmoody@pointblue.org --username ***********@pointblue.org --password ****************** --order ALL -d /mnt/data/misc


# download the ESPA order queue into the /tmp directory (fuse bucket is proving to be very unreliable).

python /home/dmoody/bulk-downloader/download_espa_order.py --email ***********@pointblue.org --username ***********@pointblue.org --password ****************** --order ALL -d /tmp

