# Classify using neural networks
library(neuralnet)
library(caret)
source("utils/load-data.r")
source("utils/accuracy-statistics.r")

AllData = LoadClassificationData()
TrainingData = LoadTrainingData()

set.seed(0xfedbeef)
folds = createFolds(AllData$cropland, 2)
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

# Try using scaled data first
Formula = paste0(paste0(ValidationNames, collapse="+"),"~",paste0(names(TrainingData), collapse="+"))
Model = neuralnet(Formula, ScaleData[fold,], 9, lifesign="full", stepmax=1000000)
plot(Model)
Prediction = compute(Model, ScaleData[-fold,names(TrainingData)])

# Normalise from min-max to sum to 100%
ScaleNNPrediction = function(prediction, global=FALSE)
{
    PredMinScale = prediction
    Min = min(prediction)
    for (i in 1:nrow(prediction))
    {
        if (!global)
            Min = min(prediction[i,])
        PredMinScale[i,] = (prediction[i,]+abs(Min)) / sum(prediction[i,]+abs(Min))*100
    }
    PredMinScale = data.frame(PredMinScale)
    names(PredMinScale) = ValidationNames
    return(PredMinScale)
}

PredMinScale = Prediction$net.result
for (i in 1:nrow(Prediction$net.result))
{
    PredMinScale[i,] = (Prediction$net.result[i,]+abs(min(Prediction$net.result[i,]))) /
        sum(Prediction$net.result[i,]+abs(min(Prediction$net.result[i,])))*100
}
PredMinScale = data.frame(PredMinScale)
names(PredMinScale) = ValidationNames

# RMSE of 24, in line with gradient boosting results (quite poor) and somewhat biased
AccuracyStatTable(PredMinScale, AllData@data[-fold,ValidationNames])

# Normalise by using cutoff
TruncateNNPrediction = function(prediction)
{
    Result = prediction
    Result[Result < 0] = 0
    Result[Result > 1] = 1
    for (i in 1:nrow(Result))
        Result[i,] = Result[i,] / sum(Result[i,])*100
    return(Result)
}
PredCutScale = Prediction$net.result
for (i in 1:length(PredCutScale))
{
    PredCutScale[[i]] = max(0, PredCutScale[[i]])
}
for (i in 1:nrow(PredCutScale))
{
    PredCutScale[i,] = PredCutScale[i,] / sum(PredCutScale[i,])*100
}
PredCutScale = data.frame(PredCutScale)
names(PredCutScale) = ValidationNames

# Still 24 RMSE, this is better for water but more biased
AccuracyStatTable(PredCutScale, AllData@data[-fold,ValidationNames])


## Retry using non-transformed data
ScaleDataUntransf = AllData@data
ScaleDataUntransf$dominant = NULL
maxs = apply(ScaleDataUntransf, 2, max)
mins = apply(ScaleDataUntransf, 2, min)
ScaleDataUntransf = as.data.frame(scale(ScaleDataUntransf, center = mins, scale = maxs - mins))

ModelUntransf = neuralnet(Formula, ScaleDataUntransf[fold,], 9, lifesign="full", stepmax=1000000)
plot(ModelUntransf)
PredictionUntransf = compute(ModelUntransf, ScaleDataUntransf[-fold,names(TrainingData)])

# No scaling: 32, bad
AccuracyStatTable(PredictionUntransf$net.result*100, AllData@data[-fold,ValidationNames])
# Row-based scaling: 23, better
AccuracyStatTable(ScaleNNPrediction(PredictionUntransf$net.result), AllData@data[-fold,ValidationNames])
# Truncating: Still 23, slightly worse
AccuracyStatTable(TruncateNNPrediction(PredictionUntransf$net.result), AllData@data[-fold,ValidationNames])
