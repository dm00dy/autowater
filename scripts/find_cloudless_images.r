# Code to pull out cloud-free images (over the valley) to use for removing false positives from cloud mask
#
#
#

baseDir <- "C:/data/landsat8"

# Load data
scn1314Df <- read.csv(file.path(baseDir, "scene_list_1314.csv"))
scn1415Df <- read.csv(file.path(baseDir, "scene_list_1415.csv"))
head(scn1314Df)
head(scn1415Df)

# Combine
scnDf <- data.frame(Filename = c(as.character(scn1314Df$Filename), as.character(scn1415Df$NewFilename)), 
                    Satellite = rep("L8"),
                    Scene = c(as.character(scn1314Df$Scene), as.character(scn1415Df$Scene)), 
                    DateAqcuired = c(scn1314Df$Date, 
                                     format(as.Date(scn1415Df$DateAcquired, format = "%m/%d/%Y"), format = "%Y%m%d")),
                    CloudCover = c(scn1314Df$CloudCover, scn1415Df$CloudCover), 
                    Mask = c(as.character(scn1314Df$MaskY_N), as.character(scn1415Df$MaskYN)))

# Cloudless
clrDf <- scnDf[scnDf$Mask == "N", ]

# Export
write.csv(clrDf, file.path(baseDir, "scene_list_clear.csv"), row.names = FALSE)
