# Classify using multiclass gradient boosting method.
library(xgboost)
library(caret)
source("utils/load-data.r")
source("utils/accuracy-statistics.r")

AllData = LoadClassificationData()
TrainData = LoadTrainingData()
ValidationData = LoadValidationData()
PureData = TrainData@data[AllData@data$pure,]
ImpureData = TrainData@data[!AllData@data$pure,]
PureLabels = AllData@data[AllData@data$pure,]$dominant
ImpureValidation = ValidationData@data[!AllData@data$pure,]

# Create an evaluation function so we can see RMSEs in progress

TrainMatrix = xgb.DMatrix(as.matrix(PureData), label=as.numeric(PureLabels)-1)
Model = xgboost(TrainMatrix, nrounds=10, objective="multi:softprob", num_class = 9)

PredRaw = predict(Model, newdata=as.matrix(ImpureData))
Prediction = matrix(PredRaw, byrow=TRUE, ncol=9, dimnames=list(list(), levels(PureLabels)))
Prediction = data.frame(Prediction)[names(ImpureValidation)]*100
# 23, better than 11%, but not by much
AccuracyStatTable(Prediction, ImpureValidation)
