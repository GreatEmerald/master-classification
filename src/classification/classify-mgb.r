# Classify using multiclass gradient boosting method.
library(xgboost)
library(caret)
source("utils/load-data.r")
source("utils/accuracy-statistics.r")

OutputDir = "../data/"

AllData = LoadClassificationData()
TrainData = LoadTrainingData()
ValidationData = LoadValidationData()
PureData = TrainData@data[AllData@data$pure,]
ImpureData = TrainData@data[!AllData@data$pure,]
PureLabels = AllData@data[AllData@data$pure,]$dominant
ImpureValidation = ValidationData@data[!AllData@data$pure,]

# Create an evaluation function so we can see RMSEs in progress

TrainMatrix = xgb.DMatrix(as.matrix(PureData), label=as.numeric(PureLabels)-1)
set.seed(0xfadedad)
Model = xgboost(TrainMatrix, nrounds=14, objective="multi:softprob", num_class = 9,
    params=list(eta=0.05, max_depth=7))

PredRaw = predict(Model, newdata=as.matrix(ImpureData))
Prediction = matrix(PredRaw, byrow=TRUE, ncol=9, dimnames=list(list(), levels(PureLabels)))
Prediction = data.frame(Prediction)[names(ImpureValidation)]*100
# 21.78, amazing
write.csv(AccuracyStatTable(Prediction, ImpureValidation), paste0(OutputDir, "stat-gradientboost.csv"))
