# Purpose: plot summary stats for 2013-2015 data, both alone and vs 2000-2011 data
#
# nelliott, Oct 2014

# Load libraries
library(ggplot2)
library(grid)
library(scales)
library(SDMTools)
library(plyr)

# Set directories
baseDir <- "C:/data/landsat/summary"
statsDir <- file.path(baseDir, "stats")
plotDir <- file.path(baseDir, "plots")

# Load and format 10-year avg data
#compDf <- read.csv(file.path(baseDir, "water_data_2000-2011_vs_2013-2015_weighted.csv"))
compDf <- read.csv(file.path(baseDir, "water_data_2000-2015_weighted_fixed.csv"))
#compDf <- rbind(compDf, data.frame(X = 1:9, Basin = unique(compDf$Basin), Month = rep("Jan"), Year = rep(2013), MonthPart = rep("whole"), SqmObserved = rep(0), SqmFlooded = rep(NA), 
#  								ObservedPercent = rep(0), WaterPercent = rep(NA), EstimatedSqmFlooded = rep(NA), ObservedHectares = rep(0), WaterHectares = rep(NA), 
#									EstimatedWaterHectares = rep(NA), EstimatedWaterHectaresWeighted = rep(NA), EstimatedWaterHectaresSdWeighted = rep(NA), MonthNum = rep(1)))
compDf$MonthNum <- match(as.character(compDf$Month), month.abb)
#compDf$MonthNum <- ifelse(compDf$MonthNum >= 7, compDf$MonthNum - 6, compDf$MonthNum + 6)
compDf$EstimatedWaterHectaresWeighted <- ifelse(is.na(compDf$EstimatedWaterHectaresWeighted), 
                                                compDf$EstimatedWaterHectares, 
                                                compDf$EstimatedWaterHectaresWeighted)
compDf$Year <- as.factor(compDf$Year)
basins <- c("Butte", "Colusa", "Sutter", "American", "Yolo", "Suisun", "Delta", "San Joaquin", "Tulare")
compDf$Basin <- factor(compDf$Basin, levels = basins)

# Clean
compDf <- subset(compDf, (compDf$ObservedPercent >= 20))
#compDf <- subset(compDf, (compDf$ObservedPercent >= 20) & !(compDf$EstimatedWaterHectaresWeighted > 105000 & as.character(compDf$Basin) %in% c("Delta", "San Joaquin", "Tulare")))

# Calculate per-basin maxes across years
#compDf <- subset(compDf, compDf$MonthPart == "whole")
compDf$Max <- mapply(FUN = function(bsn, mth) { 
  max(na.omit(compDf$EstimatedWaterHectaresWeighted[compDf$Basin == bsn & compDf$Month == mth])) 
  }, bsn = compDf$Basin, mth = compDf$Month)
compDf$Min <- mapply(FUN = function(bsn, mth) { 
  min(na.omit(compDf$EstimatedWaterHectaresWeighted[compDf$Basin == bsn & compDf$Month == mth])) 
  }, bsn = compDf$Basin, mth = compDf$Month)

# Order years
years <- c("2000-2011", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2013", "2014", "2015")
compDf$Year <- factor(as.character(compDf$Year), levels = years)

# Sort and export combined data
compDf <- compDf[with(compDf, order(Year, match(Month, month.abb), MonthPart, Basin)), ]
compDf$Source <- ifelse(compDf$Year == "2000-2011" | as.numeric(as.character(compDf$Year)) < 2012, "Landsat5", "Landsat8")
write.csv(compDf[c("Source", "Basin", "Year", "Month", "MonthPart", "ObservedHectares", "ObservedPercent", "WaterPercent", 
                   "EstimatedWaterHectaresWeighted", "EstimatedWaterHectaresSdWeighted")], file.path(baseDir, "water_data_monthly_avgs_final.csv"), row.names = FALSE)

# Remove Apr-Jun 2013
#curDf <- subset(compDf, !(compDf$Year == 2013 & as.character(compDf$Month) %in% c("Apr", "May", "Jun")))
curDf <- compDf[!(compDf$EstimatedWaterHectaresWeighted > 105000 & compDf$Year != "2000-2011"), ]
curDf <- curDf[!(curDf$MonthNum == 9 & curDf$Year == 2014 & curDf$Basin %in% c("Butte", "Sutter", "American", "Yolo")), ]
curDf <- curDf[!(curDf$MonthNum == 3 & curDf$Year == 2015 & curDf$Basin == "Tulare"), ]
curDf <- curDf[!(curDf$MonthNum == 2 & curDf$Year == 2015 & curDf$Basin == "American"), ]

# Unfactor
curDf$Basin <- as.character(curDf$Basin)
curDf$Year <- as.character(curDf$Year)
curDf$Month <- as.character(curDf$Month)
curDf$MonthPart <- as.character(curDf$MonthPart)

# Set missing data to NA
for (yr in 2000:2015) {

	for (mth in 1:12) {
		
		for (mp in c("whole", "first_half", "second_half")) {
		
			for (bsn in basins) {
			
				tempDf <- curDf[curDf$Year == yr & curDf$MonthNum == mth & curDf$MonthPart == mp & curDf$Basin == bsn, ]
				#print(tempDf)
				
				if (nrow(tempDf) == 0) {
					curDf <- rbind(curDf, c(0, bsn, yr, month.abb[mth], mp, 0, NA, NA, 0, NA, 0, NA, NA, NA, NA, mth, NA, NA, NA))
				} 
			
			}
			
		}
		
	}
	
}

# Refactor
curDf$Basin <- factor(curDf$Basin, levels = basins)
curDf$Year <- factor(as.character(curDf$Year), levels = years)

# Make numeric
curDf$MonthNum <- as.numeric(curDf$MonthNum)
curDf$EstimatedWaterHectaresWeighted <- as.numeric(curDf$EstimatedWaterHectaresWeighted)

# Plot vs all years without error bars, facet by basin
compPlot <- ggplot(data = subset(curDf, curDf$MonthPart == "whole" & curDf$Year != "2000-2011")) + geom_point(aes(x = MonthNum, y = EstimatedWaterHectaresWeighted, colour = Year)) + #use x = MonthNum to plot by calendar year and x = MonthNum to plot by bird year
  geom_line(aes(x = MonthNum, y = EstimatedWaterHectaresWeighted, colour = Year)) + facet_wrap(~Basin) #use x = MonthNum to plot by calendar year and x = MonthNum to plot by bird year
compPlot + xlab ("Month") + ylab ("Average Hectares of Open Water") + 
  ggtitle( "Average Hectares of Open Water by Basin and Month, All Years") + theme_bw() + 
  #scale_x_continuous(breaks = 1:12, labels = month.abb) + #use this line (and comment out next line) to plot by calendar year
  scale_x_continuous(breaks = 1:12, labels = month.abb) + #use this line (and comment out previous line) to plot by bird year (Jun-May)
  scale_y_continuous(labels = comma, limits = c(0, 150000), oob = rescale_none) + theme(axis.text.x = element_text(angle = 90,  vjust = 0.25))

# Save plot
plotFile <- file.path(plotDir, paste0("avg_water_by_month_all_years.jpg"))
ggsave(plotFile, width = 10, height = 7)


# Plot vs drought years without error bars, facet by basin
compPlot <- ggplot(data = subset(curDf, curDf$MonthPart == "whole" & curDf$Year %in% c("2000-2011", "2002", "2008", "2009", "2013", "2014", "2015"))) + geom_point(aes(x = MonthNum, y = EstimatedWaterHectaresWeighted, colour = Year)) + #use x = MonthNum to plot by calendar year and x = MonthNum to plot by bird year
  geom_line(aes(x = MonthNum, y = EstimatedWaterHectaresWeighted, colour = Year)) + facet_wrap(~Basin) #use x = MonthNum to plot by calendar year and x = MonthNum to plot by bird year
compPlot + xlab ("Month") + ylab ("Average Hectares of Open Water") + 
  ggtitle( "Average Hectares of Open Water by Basin and Month, Drought Years") + theme_bw() + 
  #scale_x_continuous(breaks = 1:12, labels = month.abb) + #use this line (and comment out next line) to plot by calendar year
  scale_x_continuous(breaks = 1:12, labels = month.abb) + #use this line (and comment out previous line) to plot by bird year (Jun-May)
  scale_y_continuous(labels = comma, limits = c(0, 150000), oob = rescale_none) + theme(axis.text.x = element_text(angle = 90,  vjust = 0.25))

# Save plot
plotFile <- file.path(plotDir, paste0("avg_water_by_month_2013-2015_vs_drought_years_by_basin.jpg"))
ggsave(plotFile, width = 10, height = 7)

# Plot average against 2013-2015, facet by basin
plotDf <- subset(curDf, curDf$Year %in% c("2000-2011", "2013", "2014", "2015"))
compPlot <- ggplot(data = subset(plotDf, plotDf$MonthPart == "whole")) + geom_point(aes(x = MonthNum, y = EstimatedWaterHectaresWeighted, colour = Year)) + #use x = MonthNum to plot by calendar year and x = MonthNum to plot by bird year
  geom_line(aes(x = MonthNum, y = EstimatedWaterHectaresWeighted, colour = Year)) + facet_wrap(~Basin) #use x = MonthNum to plot by calendar year and x = MonthNum to plot by bird year
compPlot + xlab ("Month") + ylab ("Average Hectares of Open Water") + 
  ggtitle( "Average Hectares of Open Water by Basin and Month, 2013-2015 vs. 2000-2011 Average") + theme_bw() + 
  #scale_x_continuous(breaks = 1:12, labels = month.abb) + #use this line (and comment out next line) to plot by calendar year
  scale_x_continuous(breaks = 1:12, labels = month.abb) + #use this line (and comment out previous line) to plot by bird year (Jun-May)
  scale_y_continuous(labels = comma, limits = c(0, 150000), oob = rescale_none) + theme(axis.text.x = element_text(angle = 90,  vjust = 0.25))

# Save plot
plotFile <- file.path(plotDir, paste0("avg_water_by_month_2013-2015_vs_2000-2011_by_basin.jpg"))
ggsave(plotFile, width = 10, height = 7)

# Plot with error bars
plotDf <- subset(curDf, curDf$Year %in% c("2000-2011", "2013", "2014", "2015"))
plotDf$EstimatedWaterHectaresSdWeighted[!is.finite(plotDf$EstimatedWaterHectaresSdWeighted)] <- NA
compEBPlot <- ggplot(data = subset(plotDf, plotDf$MonthPart == "whole")) + geom_point(aes(x = MonthNum, y = EstimatedWaterHectaresWeighted, colour = Year)) + #use x = MonthNum to plot by calendar year and x = MonthNum to plot by bird year
  geom_line(aes(x = MonthNum, y = EstimatedWaterHectaresWeighted, colour = Year)) + facet_wrap(~Basin) #use x = MonthNum to plot by calendar year and x = MonthNum to plot by bird year
compEBPlot + xlab ("Month") + ylab ("Average Hectares of Open Water") + 
  ggtitle( "Average Hectares of Open Water by Basin and Month, 2013-2015 vs. 2000-2011 Average") + theme_bw() + 
  #scale_x_continuous(breaks = 1:12, labels = month.abb) + #use this line (and comment out next line) to plot by calendar year
  scale_x_continuous(breaks = 1:12, labels = month.abb) + #use this line (and comment out previous line) to plot by bird year (Jun-May)
  scale_y_continuous(labels = comma, limits = c(0, 150000), oob = rescale_none) + theme(axis.text.x = element_text(angle = 90,  vjust = 0.25)) + #I clamped the y scale to a max of 150000 because only a couple of error bars went higher; remove the limits = c(0, 150000) to show entirety of error bars
  geom_errorbar(aes(x = MonthNum, ymax = EstimatedWaterHectaresWeighted + EstimatedWaterHectaresSdWeighted, #use x = MonthNum to plot by calendar year and x = MonthNum to plot by bird year
                    ymin = ifelse(EstimatedWaterHectaresWeighted - EstimatedWaterHectaresSdWeighted > 0, 
                                  EstimatedWaterHectaresWeighted - EstimatedWaterHectaresSdWeighted, EstimatedWaterHectaresWeighted / 20), colour = Year), width = 0.5)

# Save plot
#plotFile <- file.path(plotDir, paste0("avg_water_by_month_2013-2015_vs_2000-2011_by_basin_error_bars.jpg"))
#ggsave(plotFile, width = 10, height = 7)


# Plot with min-max
plotDf <- subset(curDf, curDf$Year %in% c("2000-2011", "2013", "2014", "2015"))
compMMPlot <- ggplot(data = subset(plotDf, plotDf$MonthPart == "whole")) + geom_point(aes(x = MonthNum, y = EstimatedWaterHectaresWeighted, colour = Year)) + #use x = MonthNum to plot by calendar year and x = MonthNum to plot by bird year
  geom_line(aes(x = MonthNum, y = EstimatedWaterHectaresWeighted, colour = Year)) + facet_wrap(~Basin) #use x = MonthNum to plot by calendar year and x = MonthNum to plot by bird year
compMMPlot + xlab ("Month") + ylab ("Average Hectares of Open Water") + 
  ggtitle( "Average Hectares of Open Water by Basin and Month, 2013-2015 vs. 2000-2011 Average") + theme_bw() + 
  #scale_x_continuous(breaks = 1:12, labels = month.abb) + #use this line (and comment out next line) to plot by calendar year
  scale_x_continuous(breaks = 1:12, labels = month.abb) + #use this line (and comment out previous line) to plot by bird year (Jun-May)
  scale_y_continuous(labels = comma, limits = c(0, 150000), oob = rescale_none) + theme(axis.text.x = element_text(angle = 90,  vjust = 0.25)) + #I clamped the y scale to a max of 150000 because only a couple of error bars went higher; remove the limits = c(0, 150000) to show entirety of error bars
  geom_errorbar(data = subset(plotDf, plotDf$Year == "2000-2011"), aes(x = MonthNum, ymax = Max, ymin = Min, colour = Year), width = 0.5)

# Save plot
#plotFile <- file.path(plotDir, paste0("avg_water_by_month_2013-2015_vs_2000-2011_by_basin_min_max.jpg"))
#ggsave(plotFile, width = 10, height = 7)

# Plot with min-max
plotDf <- subset(curDf, curDf$Year %in% c("2000-2011", "2002", "2008", "2009", "2013", "2014"))
compMMPlot <- ggplot(data = subset(plotDf, plotDf$MonthPart == "whole")) + geom_point(aes(x = MonthNum, y = EstimatedWaterHectaresWeighted, colour = Year)) + #use x = MonthNum to plot by calendar year and x = MonthNum to plot by bird year
  geom_line(aes(x = MonthNum, y = EstimatedWaterHectaresWeighted, colour = Year)) + facet_wrap(~Basin) #use x = MonthNum to plot by calendar year and x = MonthNum to plot by bird year
compMMPlot + xlab ("Month") + ylab ("Average Hectares of Open Water") + 
  ggtitle( "Average Hectares of Open Water by Basin and Month, Drought Years vs. 2000-2011 Average") + theme_bw() + 
  #scale_x_continuous(breaks = 1:12, labels = month.abb) + #use this line (and comment out next line) to plot by calendar year
  scale_x_continuous(breaks = 1:12, labels = month.abb) + #use this line (and comment out previous line) to plot by bird year (Jun-May)
  scale_y_continuous(labels = comma, limits = c(0, 150000), oob = rescale_none) + theme(axis.text.x = element_text(angle = 90,  vjust = 0.25)) + #I clamped the y scale to a max of 150000 because only a couple of error bars went higher; remove the limits = c(0, 150000) to show entirety of error bars
  geom_errorbar(data = subset(plotDf, plotDf$Year == "2000-2011"), aes(x = MonthNum, ymax = Max, ymin = Min, colour = Year), width = 0.5)

# Save plot
#plotFile <- file.path(plotDir, paste0("avg_water_by_month_drought_years_by_basin_min_max.jpg"))
#ggsave(plotFile, width = 10, height = 7)


# Individual plots by basin
for (bsn in unique(curDf$Basin)) {
  
  # Subset data
  print(paste("Creating plot for basin", bsn))
  bsnDf <- subset(curDf, curDf$Basin == bsn & curDf$Year %in% c("2000-2011", "2013", "2014"))
  
  # Plot (can stick any of the above plots in here, just change the initial data source to bsnDf from curDf)
  compBsnPlot <- ggplot(data = subset(bsnDf, bsnDf$MonthPart == "whole")) + geom_point(aes(x = MonthNum, y = EstimatedWaterHectaresWeighted, colour = Year)) + #use x = MonthNum to plot by calendar year and x = MonthNum to plot by bird year
    geom_line(aes(x = MonthNum, y = EstimatedWaterHectaresWeighted, colour = Year)) + facet_wrap(~Basin) #use x = MonthNum to plot by calendar year and x = MonthNum to plot by bird year
  compBsnPlot + xlab ("Month") + ylab ("Average Hectares of Open Water") + 
    ggtitle(paste("Average Hectares of Open Water by Month in", bsn, "Basin, 2013-2015 vs. 2000-2011 Average")) + theme_bw() + 
    #scale_x_continuous(breaks = 1:12, labels = month.abb) + #use this line (and comment out next line) to plot by calendar year
    scale_x_continuous(breaks = 1:12, labels = month.abb) + #use this line (and comment out previous line) to plot by bird year (Jun-May)
    scale_y_continuous(labels = comma) + theme(axis.text.x = element_text(angle = 90,  vjust = 0.25)) +
    # Comment out the following three lines (and plus sign at end of line above) to remove error bars from plots
    geom_errorbar(aes(x = MonthNum, ymax = EstimatedWaterHectaresWeighted + EstimatedWaterHectaresSdWeighted, #use x = MonthNum to plot by calendar year and x = MonthNum to plot by bird year
                      ymin = ifelse(EstimatedWaterHectaresWeighted - EstimatedWaterHectaresSdWeighted > 0, 
                                    EstimatedWaterHectaresWeighted - EstimatedWaterHectaresSdWeighted, EstimatedWaterHectaresWeighted / 20), colour = Year), width = 0.5)
  
  # Save plot
  plotFile <- file.path(plotDir, paste0("avg_water_by_month_2013-2015_vs_2000-2011_", bsn, ".jpg"))
  ggsave(plotFile, width = 10, height = 7)
  print(paste("Plot for basin", bsn, "exported to", plotFile))
  
}














# Standard plots: by basin and month
bsnMthYrDf <- subset(compDf, (compDf$Year == "2000-2011" | as.numeric(as.character(compDf$Year)) < 2012) & compDf$MonthPart == "whole" & compDf$MonthNum < 7)
bsnMthYrDf$Month <- factor(bsnMthYrDf$Month, levels = month.abb)
bsnMthYrDf$YearNum <- as.numeric(as.character(bsnMthYrDf$Year))
stdPlot <- ggplot(data = subset(bsnMthYrDf, bsnMthYrDf$Year == "2000-2011"), aes(x = MonthNum, y = EstimatedWaterHectaresWeighted)) + 
  geom_point() + facet_grid(~Basin)
stdPlot + xlab("Month") + ylab("Average Estimated Hectares of Open Water, 2000-2011") + 
  ggtitle("Average Estimated Hectares of Open Water in the Central Valley by Basin and Month, 2000-2011") + theme_bw() +
  scale_x_continuous(breaks = 1:6, labels = c(month.abb[1:6])) + scale_y_continuous(labels = comma) + theme(axis.text.x = element_text(angle = 90,  vjust = 0.25)) +
  geom_errorbar(aes(x = MonthNum, ymax = EstimatedWaterHectaresWeighted + EstimatedWaterHectaresSdWeighted, #use x = MonthNum to plot by calendar year and x = MonthNum to plot by bird year
                    ymin = ifelse(EstimatedWaterHectaresWeighted - EstimatedWaterHectaresSdWeighted > 0, 
                                  EstimatedWaterHectaresWeighted - EstimatedWaterHectaresSdWeighted, EstimatedWaterHectaresWeighted / 20)), width = 0.5)

# Save plot
plotFile <- file.path(plotDir, "open_water_by_month_2000-2014.jpg")
ggsave(plotFile, width = 10, height = 7)

# Standard plots: by basin and month and year
# Add blanks
withBlanksDf <- bsnMthYrDf
for (bsn in basins) {
  
  for (yr in 2000:2011) {
    
    for (mth in month.abb[1:6]) {
      
      # Check if exists and add if missing
      testDf <- subset(withBlanksDf, withBlanksDf$Basin == bsn & withBlanksDf$Year == yr & withBlanksDf$Month == mth)
      if (nrow(testDf) == 0) withBlanksDf <- rbind(withBlanksDf, c(bsn, yr, mth, match(mth, month.abb), "whole", NA, NA, NA, NA, NA, NA, NA, NA, NA, as.numeric(yr)))
      
    }
    
  }
  
}


stdPlot <- ggplot(data = subset(withBlanksDf, withBlanksDf$Year != "2000-2011"), aes(x = as.numeric(YearNum), y = as.numeric(EstimatedWaterHectaresWeighted))) + 
  geom_point() + geom_line() + facet_grid(Month~Basin)
stdPlot + xlab("Year") + ylab("Average Estimated Hectares of Open Water, Jan-Jun 2000-2011") + 
  ggtitle("Average Estimated Hectares of Open Water in the Central Valley by Basin, Month, and Year, 2000-2011") + theme_bw() +
  scale_x_continuous(breaks = 2000:2011, labels = c("2000", "", "2002", "", "2004", "", "2006", "", "2008", "", "2010", "")) +
  scale_y_continuous(labels = comma) + theme(axis.text.x = element_text(angle = 90,  vjust = 0.4))

# Save plot
plotFile <- file.path(plotDir, "open_water_by_month_and_year_2000-2014.jpg")
ggsave(plotFile, width = 10, height = 7)





