# Classify using neural networks
library(neuralnet)
library(caret)
source("utils/load-data.r")
source("utils/accuracy-statistics.r")

AllData = LoadClassificationData()
TrainingData = LoadTrainingData()

set.seed(0xfedbeef)
folds = createFolds(alldata$cropland, 2)
fold = folds$Fold1

NormalData = AllData
logitTransform = function(p) { log(p/(1-p)) }
asinTransform = function(p) { asin(sqrt(p)) }
NormalData$red = asinTransform(AllData$red)
NormalData$nir = asinTransform(AllData$nir)
NormalData$blue = asinTransform(AllData$blue)
NormalData$swir = asinTransform(AllData$swir)
NormalData$slope = asinTransform(AllData$slope)

ScaleData = NormalData@data
ScaleData$dominant = NULL
maxs = apply(ScaleData, 2, max)
mins = apply(ScaleData, 2, min)
ScaleData = as.data.frame(scale(ScaleData, center = mins, scale = maxs - mins))

Formula = paste0(paste0(ValidationNames, collapse="+"),"~",paste0(names(TrainingData), collapse="+"))
Model = neuralnet(Formula, ScaleData[fold,], 9, lifesign="full", stepmax=1000000)
plot(Model)
Prediction = compute(Model, ScaleData[-fold,names(TrainingData)])
PredScaled = Prediction$net.result
for (i in 1:nrow(Prediction$net.result))
{
    PredScaled[i,] = Prediction$net.result[i,]+abs(min(Prediction$net.result[i,])) / sum(Prediction$net.result[i,]+abs(min(Prediction$net.result[i,]))) * 100
}
AccuracyStatTable()
