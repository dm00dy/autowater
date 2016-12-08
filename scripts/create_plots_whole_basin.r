

# Load libraries
library(ggplot2)
library(grid)
library(scales)
library(SDMTools)
library(plyr)

# Directories
baseDir <- "C:/data/landsat/summary"
plotDir <- file.path(baseDir, "plots")

# Definitions
basins <- c("Butte", "Colusa", "Sutter", "American", "Yolo", "Suisun", "Delta", "San Joaquin", "Tulare")
years <- c("2000-2011", "Drought Years", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2013", "2014", "2015")

# Load data
#l5Df <- read.csv("V:/Project/climate_change/models/landsat/summary/whole_year/stats/stats_by_basin_full_year_converted.csv")

# Subset to remove decadal averages
#l5Df <- subset(l5Df, l5Df$Year != "2000-2011" & l5Df$MonthPart == "whole")
#bsnDf <- read.csv(file.path(baseDir, "combined_data_2000-2015.csv"))
bsnDf <- read.csv(file.path(baseDir, "central_valley_water_by_basin_2000-2015.csv"))

# Month numbers
bsnDf$MonthNum <- match(as.character(bsnDf$Month), month.abb)
bsnDf$MonthNumBirdYear <- ifelse(bsnDf$MonthNum >= 7, bsnDf$MonthNum - 6, bsnDf$MonthNum + 6)

# Calculate sums across valley
wholeDf <- subset(bsnDf, bsnDf$MonthPart == "whole")
cvObsDf <- aggregate(wholeDf$ObservedHectares, by = list(wholeDf$Year, wholeDf$MonthNum), FUN = sum, na.rm = TRUE)
cvFldDf <- aggregate(wholeDf$ObservedWaterHectares, by = list(wholeDf$Year, wholeDf$MonthNum), FUN = sum, na.rm = TRUE)
cvYrDf <- cbind(cvObsDf, cvFldDf$x)
names(cvYrDf) <- c("Year", "MonthNum", "ObservedHectares", "ObservedWaterHectares")

# Get relative percent and estimate total area flooded

#numbers are area of basins in sqm; divide sum by 10000 to get hectares
basinArea <- sum(3783885880.75, 2452618046.19, 972341030.782, 1900813440.86, 6176745597.42, 2386342535.03, 367971475.874, 9536478515.05, 8096826269.35) / 10000 
cvYrDf$ObservedPercent <- cvYrDf$ObservedHectares / basinArea
cvYrDf$ObservedWaterHectares[cvYrDf$Year != "2000-2011" & cvYrDf$ObservedPercent < 0.25] <- NA
cvYrDf$WaterPercent <- cvYrDf$ObservedWaterHectares / cvYrDf$ObservedHectares
cvYrDf$EstimatedWaterHectares <- cvYrDf$WaterPercent * basinArea

# Null Feb 2008 values
#subset(wholeDf, wholeDf$Year == 2008 & wholeDf$Month == "Feb")
cvYrDf$EstimatedWaterHectares[cvYrDf$Year == 2008 & cvYrDf$MonthNum == 2] <- NA

# Calculate weighted average across all years  
cvYrAvgsDf <- ddply(cvYrDf, .(MonthNum), summarize, EstimatedWaterHectares = wt.mean(EstimatedWaterHectares, ObservedPercent))
cvYrSdsDf <- ddply(cvYrDf, .(MonthNum), summarize, EstimatedWaterHectaresSd = wt.sd(EstimatedWaterHectares, ObservedPercent))
cvDecDf <- cbind(Year = rep("2000-2011"), cvYrAvgsDf, EstimatedWaterHectaresSd = cvYrSdsDf$EstimatedWaterHectaresSd)
cvDecDf$ObservedPercent <- rep(100)

# Calculate weighted average across drought years
droughtDf <- subset(bsnDf, bsnDf$Year %in% 
				c("2000-2011", "2001", "2003", "2008", "2009", "2013", "2014", "2015"))
cvDroughtAvgsDf <- ddply(droughtDf, .(MonthNum), summarize, EstimatedWaterHectares = wt.mean(EstimatedWaterHectares, ObservedPercent))
cvDroughtSdsDf <- ddply(droughtDf, .(MonthNum), summarize, EstimatedWaterHectaresSd = wt.sd(EstimatedWaterHectares, ObservedPercent))
cvDroughtDf <- cbind(Year = rep("Drought Years"), cvDroughtAvgsDf, EstimatedWaterHectaresSd = cvDroughtSdsDf$EstimatedWaterHectaresSd)
cvDroughtDf$ObservedPercent <- rep(100)

# Get sums for single years
wNewDf <- subset(wholeDf, wholeDf$Year != "2000-2011" & wholeDf$MonthPart == "whole")
cvnObsDf <- aggregate(wNewDf$ObservedHectares, by = list(wNewDf$Year, wNewDf$MonthNum), FUN = sum, na.rm = TRUE)
cvnFldDf <- aggregate(wNewDf$ObservedWaterHectares, by = list(wNewDf$Year, wNewDf$MonthNum), FUN = sum, na.rm = TRUE)
#cvnSdDf <- aggregate(wNewDf$EstimatedWaterHectaresWeightedSd, by = list(wNewDf$Year, wNewDf$MonthNum), FUN = mean, na.rm = TRUE)
#cvnDf <- cbind(cvnObsDf, cvnFldDf$x, cvnSdDf$x)
cvnDf <- cbind(cvnObsDf, cvnFldDf$x, rep(NA))
names(cvnDf) <- c("Year", "MonthNum", "ObservedHectares", "ObservedWaterHectares", "WaterHectaresSd")

# Calculate estimated area flooded
cvnDf$ObservedPercent <- cvnDf$ObservedHectares / basinArea
cvnDf$ObservedWaterHectares[cvnDf$ObservedHectares == 0] <- NA
cvnDf$ObservedWaterHectares[cvnDf$Year != "2000-2011" & cvnDf$ObservedPercent < 0.25] <- NA
cvnDf$ObservedWaterPercent <- cvnDf$ObservedWaterHectares / cvnDf$ObservedHectares
cvnDf$EstimatedWaterHectares <- cvnDf$ObservedWaterPercent * basinArea
cvnDf$EstimatedWaterHectaresSd <- cvnDf$WaterHectaresSd
cvnDf$EstimatedWaterHectaresSd[!is.finite(cvnDf$EstimatedWaterHectaresSd)] <- NA
cvnCleanDf <- cvnDf[c("Year", "MonthNum", "EstimatedWaterHectares", "EstimatedWaterHectaresSd", "ObservedPercent")]

# Combine
cvDf <- rbind(cvDecDf, cvDroughtDf, cvnCleanDf)
cvDf$MonthNumBirdYear <- ifelse(cvDf$MonthNum >= 7, cvDf$MonthNum - 6, cvDf$MonthNum + 6)

# Clean and add acres
cvDf <- subset(cvDf, cvDf$EstimatedWaterHectares < 1000000)
cvDf$EstimatedAcresFlooded <- cvDf$EstimatedWaterHectares * 2.47105

# Get min/max
cvDf$Max <- mapply(mth = cvDf$MonthNum, FUN = function(mth) { max(na.omit(cvDf$EstimatedWaterHectares[cvDf$MonthNum == mth])) })
cvDf$Min <- mapply(mth = cvDf$MonthNum, FUN = function(mth) { min(na.omit(cvDf$EstimatedWaterHectares[cvDf$MonthNum == mth])) })

# Difference from average
cvDf$DifferenceFromMean <- mapply(fld = cvDf$EstimatedWaterHectares, mth = cvDf$MonthNum, FUN = function(fld, mth) {
  fld - cvDf$EstimatedWaterHectares[cvDf$Year == "2000-2011" & cvDf$MonthNum == mth] })

cvDf$PercentDifference <- mapply(fld = cvDf$EstimatedWaterHectares, mth = cvDf$MonthNum, FUN = function(fld, mth) {
  (fld - cvDf$EstimatedWaterHectares[cvDf$Year == "2000-2011" & cvDf$MonthNum == mth]) / 
    cvDf$EstimatedWaterHectares[cvDf$Year == "2000-2011" & cvDf$MonthNum == mth]})

# Null Feb 2008 and Jan 2009; fix Sep 2014
cvDf$EstimatedWaterHectares[cvDf$Year == 2008 & cvDf$MonthNum == 2] <- NA
cvDf$DifferenceFromMean[cvDf$Year == 2008 & cvDf$MonthNum == 2] <- NA
cvDf$PercentDifference[cvDf$Year == 2008 & cvDf$MonthNum == 2] <- NA
cvDf$EstimatedWaterHectares[cvDf$Year == 2009 & cvDf$MonthNum == 1] <- NA
cvDf$DifferenceFromMean[cvDf$Year == 2009 & cvDf$MonthNum == 1] <- NA
cvDf$PercentDifference[cvDf$Year == 2009 & cvDf$MonthNum == 1] <- NA
cvDf$EstimatedWaterHectares[cvDf$Year == 2008 & cvDf$MonthNum == 2] <- NA
cvDf$DifferenceFromMean[cvDf$Year == 2008 & cvDf$MonthNum == 2] <- NA
cvDf$PercentDifference[cvDf$Year == 2008 & cvDf$MonthNum == 2] <- NA
cvDf$EstimatedWaterHectares[cvDf$Year == 2014 & cvDf$MonthNum == 9] <- cvDf$EstimatedWaterHectares[cvDf$Year == 2014 & cvDf$MonthNum == 9] / 1.5
cvDf$DifferenceFromMean[cvDf$Year == 2014 & cvDf$MonthNum == 9] <- cvDf$EstimatedWaterHectares[cvDf$Year == 2014 & cvDf$MonthNum == 9] - cvDf$EstimatedWaterHectares[cvDf$Year == "2000-2011" & cvDf$MonthNum == 9]
cvDf$PercentDifference[cvDf$Year == 2014 & cvDf$MonthNum == 9] <- cvDf$DifferenceFromMean[cvDf$Year == 2014 & cvDf$MonthNum == 9] / cvDf$EstimatedWaterHectares[cvDf$Year == "2000-2011" & cvDf$MonthNum == 9]

# Add blanks #2001, Apr & Aug; 2003, May; 2014, Jul; 
cvDf <- rbind(cvDf, c(2001, 4, NA, NA, NA, 10, NA, NA, NA, NA, NA), 
					c(2001, 8, NA, NA, NA, 2, NA, NA, NA, NA, NA),
					c(2003, 5, NA, NA, NA, 11, NA, NA, NA, NA, NA),
					c(2014, 7, NA, NA, NA, 1, NA, NA, NA, NA, NA))

# Prepare for export
cvOutDf <- subset(cvDf, !is.na(cvDf$DifferenceFromMean))
cvOutDf$Month <- month.abb[round(cvOutDf$MonthNum, 0)]
cvOutDf$ObservedPercent <- round(cvOutDf$ObservedPercent * 100, 2)
cvOutDf$PercentDifference <- round(cvOutDf$PercentDifference * 100, 2)
cvOutDf$EstimatedWaterHectares <- round(cvOutDf$EstimatedWaterHectares, 0)
cvOutDf$DifferenceFromMean <- round(cvOutDf$DifferenceFromMean, 0)
cvOutDf <- cvOutDf[c("Year", "Month", "ObservedPercent", "EstimatedWaterHectares", "DifferenceFromMean", "PercentDifference")]
cvOutDf <- subset(cvOutDf, cvOutDf$Year %in% c(2001, 2002, 2003, 2008, 2009, 2013, 2014, 2015))
write.csv(cvOutDf, file.path(baseDir, "difference_from_mean_central_valley.csv"), row.names = FALSE)

# Central Valley plot, all years as points, 2013-2015 pulled out, envelope
cvPlot <- ggplot(data = cvDf) + geom_ribbon(data = subset(cvDf, cvDf$Year == "2000-2011"), aes(x = MonthNum, ymax = Max, ymin = Min), fill = "#FFCCCC", width = 0) +
  geom_point(data = subset(cvDf, cvDf$Year %in% c("2000-2011", "2013", "2014", "2015")), aes(x = MonthNum, y = EstimatedWaterHectares, colour = Year), size = 3) +
  geom_line(data = subset(cvDf, cvDf$Year %in% c("2000-2011", "2013", "2014", "2015")), aes(x = MonthNum, y = EstimatedWaterHectares, colour = Year), size = 1)
cvPlot + xlab ("Month") + ylab ("Average Hectares of Open Water") + 
  ggtitle("Average Hectares of Open Water in the Central Valley by Month, 2013-2015 vs 2000-2011") + theme_bw() + 
  scale_x_continuous(breaks = 1:12, labels = month.abb) + 
  scale_y_continuous(labels = comma) + theme(axis.text.x = element_text(angle = 90,  vjust = 0.25))

# Save plot
plotFile <- file.path(plotDir, "avg_water_by_month_2013-2015_vs_all_years_envelope.jpg")
ggsave(plotFile, width = 10, height = 7)


# Plot as a difference from mean
cvDifPlot <- ggplot(data = subset(cvDf, cvDf$Year %in% c("2000-2011", "2013", "2014", "2015")), aes(x = MonthNum, y = DifferenceFromMean, colour = Year)) + geom_point() + geom_line()
cvDifPlot  +xlab ("Month") + ylab ("Difference in Average Hectares of Open Water") + 
  ggtitle("Estimated Hectares of Open Water in the Central Valley, 2013-2015 vs 2000-2011 Average") + theme_bw() + 
  scale_x_continuous(breaks = 1:12, labels = month.abb) + scale_y_continuous(labels = comma) 

# Save plot
plotFile <- file.path(plotDir, "difference_avg_water_by_month_2013-2015_vs_all_years.jpg")
ggsave(plotFile, width = 10, height = 7)


# Plot each drought year as a percent difference from mean
#Jul 2001 - Jun 2002, 3. Jul 2008-Jun 2009, 4. July 2013-Jun 2014
cvDifPlot <- ggplot(data = subset(cvDf, cvDf$Year %in% c("2000-2011", "Drought Years", "2001", "2003", "2008", "2009", "2013", "2014", "2015")), 
                    aes(x = MonthNum, y = PercentDifference, colour = Year)) + geom_point() + geom_line()
cvDifPlot  +xlab ("Month") + ylab ("Difference in Average Hectares of Open Water") + 
  ggtitle(paste("Estimated Hectares of Open Water in the Central Valley, Drought Years vs 2000-2011 Average")) + theme_bw() + 
  scale_x_continuous(breaks = 1:12, labels = month.abb) + scale_y_continuous(labels = percent) 

# Save plot
plotFile <- file.path(plotDir, "difference_percent_avg_water_by_month_drought_years_vs_all_years.jpg")
ggsave(plotFile, width = 10, height = 7)


# Plot 2013, 2014, 2015 as a percent difference from mean
cvDifPlot <- ggplot(data = subset(cvDf, cvDf$Year %in% c("2000-2011", "2013", "2014", "2015")), aes(x = MonthNum, y = PercentDifference, colour = Year)) + geom_point() + geom_line()
cvDifPlot  +xlab ("Month") + ylab ("Percent Difference in Average Hectares of Open Water") + 
  ggtitle(paste("Percent Difference in of Open Water in the Central Valley, 2013-2015 vs 2000-2011 Average")) + theme_bw() + 
  scale_x_continuous(breaks = 1:12, labels = month.abb) + scale_y_continuous(labels = percent) 

# Save plot
plotFile <- file.path(plotDir, "difference_percent_avg_water_by_month_2013-2015_vs_all_years.jpg")
ggsave(plotFile, width = 10, height = 7)

##########################################
## Average Difference for Drought Years ##
##########################################

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

# Plot drought year average as a percent difference from mean
drPlotDf <- subset(cvDf, cvDf$Year %in% c("2000-2011", "Drought Years", "2001", 
						"2003", "2008", "2009", "2013", "2014", "2015"))
cvDifPlot <- ggplot(subset(drPlotDf, drPlotDf$Year %in% c("2000-2011", "Drought Years")),
							aes(x = MonthNum, y = PercentDifference, colour = Year)) + 
				geom_point(size = 3) + 
				geom_line(data = subset(drPlotDf, drPlotDf$Year %in% c("2000-2011", "Drought Years")), 
							aes(x = MonthNum, y = PercentDifference, colour = Year), size = 1) +
				geom_point(data = subset(drPlotDf, !(drPlotDf$Year %in% c("2000-2011", "Drought Years"))),
				aes(x = MonthNum, y = PercentDifference, colour = "Individual\nDrought Years"))
cvDifPlot  +xlab ("Month") + ylab ("Difference in Average Hectares of Open Water") + 
	scale_colour_manual(values = c(gg_color_hue(1), gg_color_hue(2)[2], "grey")) + 
	ggtitle(paste("Estimated Hectares of Open Water in the Central Valley, Drought Average vs 2000-2011 Average")) + theme_bw() + 
	scale_x_continuous(breaks = 1:12, labels = month.abb) + scale_y_continuous(labels = percent) 

# Save plot
plotFile <- file.path(plotDir, "difference_percent_avg_water_by_month_drought_avg_vs_all_years.jpg")
ggsave(plotFile, width = 10, height = 7)



#######################
# Difference by Basin #
#######################

# Unfactor
wholeDf$Basin <- as.character(wholeDf$Basin)
wholeDf$Year <- as.character(wholeDf$Year)
wholeDf$Month <- as.character(wholeDf$Month)
wholeDf$MonthPart <- as.character(wholeDf$MonthPart)	

# Set missing data to NA
for (yr in 2000:2015) {

	for (mth in 1:12) {
		
		for (mp in c("whole")) {
		
			for (bsn in basins) {
			
				tempDf <- wholeDf[wholeDf$Year == yr & wholeDf$MonthNum == mth & wholeDf$MonthPart == mp & wholeDf$Basin == bsn, ]
				#print(tempDf)
				
				if (nrow(tempDf) == 0) {
					wholeDf <- rbind(wholeDf, c(bsn, yr, month.abb[mth], mp, 0, NA, NA, 0, NA, 0, NA, NA, mth, NA))
				} 
			
			}
			
		}
		
	}
	
}

# Refactor
wholeDf$Basin <- factor(wholeDf$Basin, levels = basins)
wholeDf$Year <- factor(as.character(wholeDf$Year), levels = years)

# Make numeric
wholeDf$MonthNum <- as.numeric(wholeDf$MonthNum)
wholeDf$EstimatedWaterHectares <- as.numeric(wholeDf$EstimatedWaterHectares)
wholeDf$ObservedPercent <- as.numeric(wholeDf$ObservedPercent)

# Clean weighted averages and sds
l5Df <- subset(wholeDf, as.numeric(as.character(wholeDf$Year)) <= 2011 & wholeDf$Year != "2000-2011")
l5Df <- l5Df[!(l5Df$EstimatedWaterHectares > 105000 & l5Df$Year != "2000-2011"), ]
avgsDf <- ddply(l5Df, .(Basin, MonthNum, MonthPart), summarize, EstimatedWaterHectares = wt.mean(EstimatedWaterHectares, ObservedPercent))
avgsDf <- na.omit(avgsDf)
wholeYrDf <- subset(wholeDf, wholeDf$Year != "2000-2011")
wholeYrDf <- wholeYrDf[!(wholeYrDf$MonthNum == 9 & wholeYrDf$Year == 2014 & wholeYrDf$Basin %in% c("Butte", "Sutter", "American", "Colusa", "Yolo")), ]
wholeYrDf <- wholeYrDf[!(wholeYrDf$MonthNum == 3 & wholeYrDf$Year == 2015 & wholeYrDf$Basin == "Tulare"), ]
wholeYrDf <- wholeYrDf[!(wholeYrDf$MonthNum == 2 & wholeYrDf$Year == 2015 & wholeYrDf$Basin == "American"), ]

# Difference from average
wholeYrDf$DifferenceFromMean <- mapply(fld = wholeYrDf$EstimatedWaterHectares, mth = wholeYrDf$MonthNum, bsn = wholeYrDf$Basin, FUN = function(fld, mth, bsn) {
  fld - avgsDf$EstimatedWaterHectares[avgsDf$MonthNum == mth & avgsDf$Basin == bsn] })

wholeYrDf$PercentDifference <- mapply(fld = wholeYrDf$EstimatedWaterHectares, mth = wholeYrDf$MonthNum, bsn = wholeYrDf$Basin, FUN = function(fld, mth, bsn) {
  (fld - avgsDf$EstimatedWaterHectares[avgsDf$MonthNum == mth & avgsDf$Basin == bsn]) / 
    avgsDf$EstimatedWaterHectares[avgsDf$MonthNum == mth & avgsDf$Basin == bsn]})


difDf <- wholeYrDf[c("Basin", "Year", "Month", "MonthNum", "ObservedPercent", "EstimatedWaterHectares", "DifferenceFromMean", "PercentDifference")]

# Add in blanks for 2000-2011
blankDf <- data.frame(Basin = rep(basins, 12), Year = rep("2000-2011"), 
                      Month = rep(month.abb, each = 9), MonthNum = rep(c(1:12), each = 9), ObservedPercent = rep(0),
                      EstimatedWaterHectares = rep(NA), DifferenceFromMean = rep(0), PercentDifference = rep(0))
difDf <- rbind(difDf, blankDf)

# Remove bad percent differences
difDf$PercentDifference[difDf$PercentDifference < -.99 & !is.na(difDf$PercentDifference)] <- -.8

# Order
difDf$Year <- factor(as.character(difDf$Year), levels = 
                       c("2000-2011", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2013", "2014", "2015"))
difDf$Month <- factor(as.character(difDf$Month), levels = month.abb)
difDf$Basin <- factor(as.character(difDf$Basin), levels = basins)
difDf <- difDf[with(difDf, order(Year, Month, Basin)), ]
difOutDf <- subset(difDf, !is.na(difDf$DifferenceFromMean))
difOutDf$ObservedPercent <- round(difOutDf$ObservedPercent, 2)
difOutDf$PercentDifference <- round(difOutDf$PercentDifference * 100, 2)
difOutDf$EstimatedWaterHectares <- round(difOutDf$EstimatedWaterHectares, 0)
difOutDf$DifferenceFromMean <- round(difOutDf$DifferenceFromMean, 0)
difOutDf <- difOutDf[c("Year", "Month", "ObservedPercent", "EstimatedWaterHectares", "DifferenceFromMean", "PercentDifference")]
difOutDf <- subset(difOutDf, difOutDf$Year %in% c("2000-2011", 2001, 2002, 2003, 2008, 2009, 2013, 2014, 2015))
write.csv(difOutDf, file.path(baseDir, "difference_from_mean_by_basin.csv"), row.names = FALSE)

# Plot
bsnDifPlot <- ggplot(data = subset(difDf, difDf$Year %in% c("2000-2011", "2013", "2014", "2015")), aes(x = MonthNum, y = PercentDifference, colour = Year)) + 
  geom_point() + geom_line() + facet_wrap(~Basin)
bsnDifPlot + xlab ("Month") + ylab ("Percent Difference in Average Hectares of Open Water") + 
  ggtitle("Percent Difference in Open Water by Basin in the Central Valley, 2013-2015 vs 2000-2011 Average") + theme_bw() + 
  scale_x_continuous(breaks = 1:12, labels = month.abb) + scale_y_continuous(labels = percent)  + theme(axis.text.x = element_text(angle = 90,  vjust = 0.25))

# Save plot
plotFile <- file.path(plotDir, "difference_percent_avg_water_by_month_by_basin_2013-2015_vs_all_years.jpg")
ggsave(plotFile, width = 10, height = 7)

