# autowater

Overview

This repository contains code used by Point Blue Conservation Science to classify Landsat 8 imagery of California's Central Valley for the presence of surface water.  Code is still in development.

Standard Disclaimer

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Background

The Central Valley is a nexus for water resources in California, draining the Sacramento River and San Joaquin River watersheds (Hanak and Lund 2012). Future conditions will challenge California’s water system and require informed and innovative adaptation strategies (Medellin-Azuara et al. 2008, Lund et al. 2007). A better understanding of the distribution and abundance of open surface water was cited in the 2009 update of California’s Water Plan (DWR 2009) as necessary to improve water management.

The Landsat satellite mission delivers imagery every 16 days from nearly every place on the earth at a high spatial resolution (30m pixels). We have developed a system that (1) automated the classification process of open water in the Central Valley using a newly developed open water classification model in conjunction with Landsat 8 and (2) provides access to and summaries of these data in near real-time to help water managers throughout the Central Valley and all of California.

Methods

   Download. We have developed and implemented an unattended framework to automatically harvest Landsat 8 imagery as they are made available in the Landsat data repository (http://earthexplorer.usgs.gov/). We download all images from the four Landsat scenes that compose the majority of the Central Valley. Each processed and compressed image file contains twelve bands: nine from the Operational Land Imager, two from the Thermal Infrared Sensor, and one Quality Assessment (QA) band created by USGS during processing. Initially processed Landsat images are generally available for download 2-4 days after image acquisition. Each day at 1200 AM UTC, an automated script logs into the EROS Science Processing Architecture (ESPA) server, initiates an authenticated session, and submits an order for each of the 4 scenes. If no scene was acquired that day, no further action is taken. If a scene was acquired, then the scene is added to a download queue, and is generally made available when NASA completes the scene post-processing in 3-4 days. In parallel, an automated download script checks the queue for completed scenes and downloads any new data. The system assumes that some login attempts will fail due to network congestion or server maintenance at ESPA, and therefore submits an order for the current date as well as the three previous days in each iteration.
   
Store. We have established an account to store and process imagery in the Google cloud which provides flexibility in storage and processing needs over time and allows an interface with the Google Earth Engine. Specifically we have provisioned cloud storage “buckets” in the Google network where we are staging raster files for processing. Normalization and classification of the raster images is performed on Google’s Compute Engine hardware; file transfer from cloud storage to compute nodes within the Google network is fast and cheap.

Process. We have automated the processing of the Landsat 8 imagery using scripts written in R and Python. We download the pre-processed normalized Climate Data Records (CDR) from NASA/USGS and after detecting that new imagery has been successfully downloaded, our automated system, with code processing the imagery largely in the Google cloud initiates the classification prediction process (Figure 1).

a. Standardize image extent and resolution. Images of the same scene have a slightly different spatial extent on each flyover so we first clip and re-sample images to a standard extent and resolution.

b. Mask images for clouds. If there are clouds in the study area, as indicated by the QA band, all pixels within 1-km of a pixel identified as having a medium or high probability of being cloudy are turned to “No data”. The 1-km buffer captures and removes poorly-defined cloud edges and cloud shadows.

c. Classify images. Each image is classified on a pixel-by-pixel basis as either being open water, not open water, or an area of no data (usually because of cloud cover) using a Boosted Regression Tree classification model for the CDR layers developed as part of this project. This is the similar techniques used by Reiter et al. (2015). Our model was trained on >10,000 water ground truth points from aerial surveys and ground surveys collected in July 2013 – April 2015. After the classified water layer is generated, it is uploaded to Google Earth Engine where areas of water determined to be highly vegetated (using the Normalized Difference Vegetation Index; see Reiter et al. 2015) are removed as not being open water. Overall, the existing classification approach predicts extremely well (Area-Under-the-Curve = 0.96 where 1= perfect prediction, 0.5 = poor prediction [random]).

d. Create grids files of water coverage. Final layers are turned into GeoTiff files and Google Earth compatible .kml files to enable download and display. Because the date of image acquisition varies by up to a week across the four scenes that make up the Central Valley, each scene is ultimately included in two different mosaics.

Here is how this works for a single Landsat cycle of 16 days:

Day 1 – first day of Landsat cycle.
   
Day 5 – scenes 1 and 2 (p44r33 and p44r34) acquired
   
Day 7 – classified data from scenes 1 and 2 available; scene 3 (p42r35) acquired.
   
Day 9 – classified data from scene 3 available; most recent data from all four scenes mosaicked (Day 14 of previous cycle through Day 7 of current cycle)
   
Day 14 – scene 4 (p43r44) acquired
   
Day 16 – last day of Landsat cycle. Classified data from scene 4 available; mosaic created from most recent image of all four scenes (acquired on Days 5 – 14 of current cycle).
   
Quality Control.
    
Because errors can occur while downloading, during standardization, or even as part of the model-based classification, we developed an automated error checking process. We have several mechanisms that are used to flag errors and particularly take advantage of our existing data from 2000-2015 to establish thresholds for when an error has occurred. First, we have developed an error-handler to catch and manage known procedural issues during the initial retrieval of the imagery including interrupted downloads and corrupted or incomplete files. We then check for errors in our processing after each of three critical steps: (1) after standardization; (2) after prediction, and (3) after mosaicking. After standardization, we evaluate the distribution of cloud free areas against the distribution of these values from the same scene and month from previous images. After prediction, we have developed a set of known and invariant water (e.g., centers of rivers and canals) and non-water locations (e.g., hillsides, fields that are not flooded) from throughout the Central Valley and check those against the predictions for those locations. 
    
Images are assigned a quality score based on the percentage of points they correctly classify and images below a certain threshold are flagged for manual review. Though the locations selected are generally invariant, we do anticipate some change over time and will update these points accordingly. After mosaicking the images together for full Central Valley coverage, we follow methods in Reiter et al. (2015) and summarize the total extent of open surface water (ha) by Central Valley Joint Venture planning basin (JV basin) and for the whole Central Valley. Our system then flags open water estimates that fall outside the minimum or maximum observed values during the same period and region between 2000 and 2015. The whole process is tracked, with each step logged, to inform the system and users of the status of all images downloaded. The system emails a status report to a technician upon encountering a serious error and/or after successfully completing processing.
    
Quality metadata. A data quality table is automatically populated reporting each image used, the date the image was taken, the proportion that was cloud-free after scenes are fully masked, and a relative quality score based on quality control metrics described above that is populated after each image is completed.
    
Data Summaries. As each mosaicked image is produced, we generate data summaries for the entire Central Valley and for individual planning Basins used by the Central Valley Joint Venture. Summaries include (1) total ha of open surface water and (2) the proportion of managed wetlands, rice, corn, and other waterbird compatible crops that are flooded.
    
Distribution. The final classified water outputs are made available via the system webpage and can be viewed on the map viewer. 

LITERATURE CITED

DWR, 2009. California Water Plan Update 2009. California Department of Water Resources, Sacramento, California, USA.

Hanak, E., and J.R. Lund, 2012. Adapting California’s Water Management to Climate Change. Climatic Change 111:17-44.

Lund J.R., E. Hanak, W. Fleenor, R. Howitt, J. Mount, and P. Moyle, 2007. Envisioning Futures for the Sacramento–San Joaquin Delta. Public Policy Institute of California. San Francisco, California.

Medellín–Azuara, J., J.J. Harou, M.A. Olivares, K. Madani, J.R. Lund, R.E. Howitt, S.Tanaka, M.W. Jenkins, T. Zhu, 2008. Adaptability and Adaptations of California’s Water Supply System to Dry Climate Warming. Climatic Change 87(Suppl 1):S75–S90.

Reiter, M.E., N. K. Elliott, S. Veloz, D. Jongsomjit, C. M. Hickey, M. Merrifeld, and M. D. Reynolds. 2015. Spatio-Temporal Patterns of Open Surface Water in the Central Valley of California 2000–2011: Drought, Land Cover, and Waterbirds. Journal of American Water Resources Association. 51(6): 1722–1738
