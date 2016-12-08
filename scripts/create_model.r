# Purpose: model water cover
#
#

# Packages
library(gbm) #gbm
library(dismo) #gbm.step
library(SDMTools) #confusion.matrix

# Set directories
baseDir <- "/home/blue"
dataDir <- file.path(baseDir, "pvt/water_data/attributed")
tempDir <- filepath(baseDir, "processing")
codeDir <- file.path(baseDir, "sparklemotion/autowater/functions")
modelDir <- file.path(baseDir, "pvt/model")

# Buckets
pvtBkt <- "gs://pointblue-autowater-pvt"
dataBkt <- file.path(pvtBkt, "water_data/attributed")
modelBkt <- file.path(pvtBkt, "model")

# Load code
source(file.path(codeDir, "loading_code_independent.r"))
sourceDir(codeDir)

# Combine water data
waterFiles <- list.files(dataDir, pattern = ".csv", full.names = TRUE)
waterDf <- do.call("rbind", lapply(waterFiles, read.csv, header = TRUE))
waterDf <- waterDf[, !(names(waterDf) %in% c("X", "X.1"))]

# Export and copy to cloud
waterFn <- "water_data_combined.csv"
waterTemp <- file.path(tempdir(), waterFn)
write.csv(waterDf, waterTemp, row.names = FALSE)
gsCopy(waterTemp, file.path(dataBkt, waterFn), copySem = FALSE, deleteFrom = TRUE)

# Read combined water data
waterDf <- read.csv(file.path(dataDir, waterFn))

# Main body
# -------------------------------------------------

# Assign a weight to each observation that is inversely proportional to the number of days off
# In this case, weight inversely by distance, adding one to days off so that 0 days off produces a value of 1 rather than infinity
waterDf$Weights <- 1 / (waterDf$DaysOff + 1)^2
#waterDf$Weights[waterDf$Project == "Satellite Classification"] <- 0.25

# Remove middle of the road numbers
waterDf <- waterDf[waterDf$WaterYN != -1, ]

# Divide into training and testing dfs
set.seed(111)
indexes <- sample(1:nrow(waterDf), size = 0.8 * nrow(waterDf))
trainDf <- waterDf[indexes, ]
testDf <- waterDf[-indexes, ]

## Run models##
#brtWeights <- gbm.step(data = trainDf, gbm.x = 6:16, gbm.y = 18, site.weights = trainDf$ModelWeights, learning.rate = learningRate, tree.complexity = treeComplexity, family = modelFamily, max.trees = maxTrees)
brt <- gbm.step(data = trainDf, gbm.x = 7:18, gbm.y = 3, site.weights = trainDf$Weights, 
				learning.rate = 0.01, tree.complexity = 3, family = "bernoulli", max.trees = 10000)


## Test predictions against observations
#testDf$PredictedWater <- predict(brtWeights, testDf, n.trees = brtWeights$n.trees, type = "response")
testDf$WaterPredicted <- predict(brt, testDf, n.trees = brt$n.trees, type = "response")

# Save model objects
modelFn <- "landsat8_brt.RData"
brtTemp <- file.path(tempDir, modelFn)
save(list = c("brt", "waterDf", "testDf", "trainDf", "indexes"), file = brtTemp)
gsCopy(brtTemp, file.path(modelBkt, modelFn), copySem = FALSE, deleteFrom = TRUE)

# Load model
modelFn <- "landsat8_brt.RData"
load(file.path(modelDir, modelFn))

# Function to calculate a threshold and an AUC given vectors of predicted and observed values
# Arguments:
#	predicted: Vector of predicted outcomes, should be continuous 0-1 and of the same length as observed
#	observed: Vector of observed outcomes, should be binary 0/1 and of the same length as predicted
#
# Returns:
# 	A list of length three with the AUC, threshold, and confusion matrix
calcThresh <- function(predicted, observed) {
	
	# Libraries
	require(ROCR)

	# Predict
	pre <- prediction(predicted, observed)
	
	# AUC
	per <- performance(pre, 'auc')
	auc <- per@y.values[[1]] 
	message(paste("AUC:", auc))
	
	# Threshold
	perf <- performance(pre, "tpr", "fpr")
	fpr <- perf@x.values[[1]]
	tpr <- perf@y.values[[1]]
	sum1 <- tpr + (1 - fpr)
	index <- which.max(sum1)
	cutoff <- perf@alpha.values[[1]][[index]]
	message(paste("Threshold:", cutoff))
	
	# Confustion matrix
	cnf <- confusion.matrix(observed, predicted, cutoff)
	message("Confusion Matrix:")
	print(cnf)
	
	# Return
	return(list("AUC" = auc, "Threshold" = cutoff, "ConfusionMatrix" = cnf))

}


stats <- calcThresh(testDf$WaterPredicted, testDf$WaterYN)

waterDf$WaterPredicted <- predict(brt, waterDf, n.trees = brt$n.trees, type = "response")
stats2 <- calcThresh(waterDf$WaterPredicted, waterDf$WaterYN)

confusion.matrix(testDf$WaterYN, testDf$WaterPredicted, 0.18)

'
> stats
$AUC
[1] 0.9804817

$Threshold
[1] 0.1552574

$ConfusionMatrix
obs
pred    0   1
0 4249  77
1  147 939
attr(,"class")
[1] "confusion.matrix"
'

#[1] "AUC: 0.98487420870291"
#[1] "Threshold: 0.0481883753349531"
#[1] "Confusion Matrix:"
#    obs
#pred    0   1
#   0 2009  48
#   1  106 734
#attr(,"class")
#[1] "confusion.matrix"

statsSO <- calcThresh(testDf$PredictedWaterSurveyOnly, testDf$ObservedWaterBinary)
statsMO <- calcThresh(testDf$PredictedWaterManualOnly, testDf$ObservedWaterBinary)

ggplot(testDf, aes (x = as.factor(ObservedWaterBinary), y = PredictedWaterSurveyAndManual, fill = as.factor(Project))) + geom_boxplot()
ggplot(testDf, aes (x = as.factor(ObservedWaterBinary), y = PredictedWaterSMWeighted, fill = as.factor(Project))) + geom_boxplot()

ggplot(testDf, aes (x = as.factor(ObservedWaterBinary), y = PredictedWaterSurveyOnly, fill = as.factor(Project))) + geom_boxplot()
ggplot(testDf, aes (x = as.factor(ObservedWaterBinary), y = PredictedWaterManualOnly, fill = as.factor(Project))) + geom_boxplot()


# Calculate whether prediction is good or bad
testDf$PredictedWaterBinary <- ifelse(testDf$PredictedWater >= cutoff, 1, 0)
testDf$GoodPrediction <- ifelse(testDf$ObservedWaterBinary == testDf$PredictedWaterBinary, TRUE, FALSE)
#ggplot(testDf, aes(x = GoodPrediction, y = DaysOff)) + geom_point(position = "jitter")
ggplot(testDf, aes(x = GoodPrediction, y = DaysOff, fill = Project)) + geom_boxplot()
