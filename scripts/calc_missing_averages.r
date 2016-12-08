# Calculate missing 2000-2011 averages

# Load libraries
library(plyr)
library(SDMTools)

# Set directories
baseDir <- "C:/data/landsat/summary"

# Load data
wDf <- read.csv(file.path(baseDir, "water_data_2000-2015_weighted.csv"))

# Calculate averages
yrDf <- subset(wDf, wDf$Year != "2000-2011")
wAvgsDf <- ddply(yrDf, .(Basin, Month, MonthPart), summarize, 
	EstimatedWaterHectaresWeighted = wt.mean(EstimatedWaterHectares, ObservedPercent))

# Combine
decDf <- cbind(wAvgsDf, Year = rep("2000-2011", nrow(wAvgsDf)),
				ObservedHectares = rep(NA, nrow(wAvgsDf)), 
				ObservedPercent = rep(100, nrow(wAvgsDf)), 
				ObservedWaterHectares = rep(NA, nrow(wAvgsDf)), 
				WaterPercent = rep(NA, nrow(wAvgsDf)), 
				EstimatedWaterHectares = wAvgsDf$EstimatedWaterHectaresWeighted)
				
cols <- c("Basin", "Year", "Month", "MonthPart", "ObservedHectares", "ObservedPercent",
			"ObservedWaterHectares", "WaterPercent", "EstimatedWaterHectares",
			"EstimatedWaterHectaresWeighted")
wOutDf <- rbind(decDf[cols], yrDf[cols])

# Export
write.csv(wOutDf, file.path(baseDir, "water_data_2000-2015_weighted_fixed.csv"), 
			row.names = FALSE)
			