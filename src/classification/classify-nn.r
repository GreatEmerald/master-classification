# Classify using neural networks
library(neuralnet)
library(caret)
source("utils/load-data.r")
source("utils/accuracy-statistics.r")

AllData = LoadClassificationData()
TrainingData = LoadTrainingData()
TrainingNames = GetTrainingNames(exclude=c("osavi", "amplitude1"))

OutputDir = "../data/"

set.seed(0xfedbeef)
folds = createFolds(AllData$cropland, 4)
fold = folds$Fold1

# Naive attempt at raw data
Formula = paste0(paste0(GetValidationNames(), collapse="+"),"~",paste0(TrainingNames, collapse="+"))
Model = neuralnet(Formula, AllData@data[fold,], 12, lifesign="full", stepmax=100000, rep=5, threshold=10)
plot(Model)
Prediction = compute(Model, AllData@data[-fold,names(TrainingData)])

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
    names(PredMinScale) = GetValidationNames()
    return(PredMinScale)
}

PredMinScale = Prediction$net.result
for (i in 1:nrow(Prediction$net.result))
{
    PredMinScale[i,] = (Prediction$net.result[i,]+abs(min(Prediction$net.result[i,]))) /
        sum(Prediction$net.result[i,]+abs(min(Prediction$net.result[i,])))*100
}
PredMinScale = data.frame(PredMinScale)
names(PredMinScale) = GetValidationNames()

# RMSE of 24, in line with gradient boosting results (quite poor) and somewhat biased
AccuracyStatTable(PredMinScale, AllData@data[-fold,GetValidationNames()])

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
names(PredCutScale) = GetValidationNames()

# Still 24 RMSE, this is better for water but more biased
AccuracyStatTable(PredCutScale, AllData@data[-fold,GetValidationNames()])


## Retry using scaled data
ScaleDataUntransf = AllData@data
ScaleDataUntransf$dominant = NULL
maxs = apply(ScaleDataUntransf, 2, max)
mins = apply(ScaleDataUntransf, 2, min)
ScaleDataUntransf = as.data.frame(scale(ScaleDataUntransf, center = mins, scale = maxs - mins))

NNCV = function(filename=paste0(OutputDir, "stat-neuralnetworks.csv"), exclude=c(), ...)
{
    TrainingNames = GetTrainingNames(exclude=exclude)
    Formula = paste0(paste0(GetValidationNames(), collapse="+"),"~",paste0(TrainingNames, collapse="+"))
    PredictionsPerFold = data.frame()
    for (f in 1:length(folds))
    {
        set.seed(0xfedbeef)
        ModelUntransf = neuralnet(Formula, ScaleDataUntransf[-folds[[f]],], lifesign="minimal", rep=10, ...)
        PredictionUntransf = compute(ModelUntransf, ScaleDataUntransf[folds[[f]],TrainingNames], rep=which.min(ModelUntransf$result.matrix[1,]))
    
        if (nrow(PredictionsPerFold) == 0)
            PredictionsPerFold = ScaleNNPrediction(PredictionUntransf$net.result)
        else
            PredictionsPerFold = rbind(PredictionsPerFold, ScaleNNPrediction(PredictionUntransf$net.result))
    }
    Validator = AllData@data[unlist(folds),GetValidationNames()]
    AST = AccuracyStatTable(PredictionsPerFold, Validator)
    print(AST)
    plot(unlist(PredictionsPerFold), unlist(Validator))
    write.csv(AST, paste0(OutputDir, "stat-neuralnetworks.csv"))
}
# Unoptimised
NNCV(paste0(OutputDir, "stat-neuralnetworks-unoptimised.csv"), hidden=11, threshold=0.15)
# Optimised
NNCV(exclude=c("osavi", "amplitude1", "aspect", "blue", "is.water", "amplitude2", "tpi"), hidden=9, threshold=0.10)
# 0.15: 22.75

# No scaling: 24.5
#AccuracyStatTable(PredictionUntransf$net.result*100, AllData@data[-fold,GetValidationNames()])
# Row-based scaling: 22.3, better
#AccuracyStatTable(ScaleNNPrediction(PredictionUntransf$net.result), AllData@data[-fold,GetValidationNames()])
# Truncating: 21.8, slightly better than scaling
#AccuracyStatTable(TruncateNNPrediction(PredictionUntransf$net.result), AllData@data[-fold,GetValidationNames()])
