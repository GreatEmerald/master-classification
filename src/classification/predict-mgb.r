# Predict a whole tile using extreme gradient boosting

library(xgboost)
library(raster)
source("utils/set-temp-path.r")
source("utils/load-data.r")

OutputDir = "../../userdata/predictions/"

AllData = LoadClassificationData()
TrainData = LoadTrainingData()
PureData = TrainData@data[AllData@data$pure,]
PureLabels = AllData@data[AllData@data$pure,]$dominant
TrainingRasters = LoadTrainingRasters()

TrainMatrix = xgb.DMatrix(as.matrix(PureData), label=as.numeric(PureLabels)-1)
Model = xgboost(TrainMatrix, nrounds=14, objective="multi:softprob", num_class = 9,
    params=list(eta=0.05, max_depth=7))

GBP = function(Model, blockvals, ...)
{
    PredRaw = predict(Model, newdata=as.matrix(blockvals), ...)
    Prediction = matrix(PredRaw, byrow=TRUE, ncol=9, dimnames=list(list(), levels(PureLabels)))
    Prediction = data.frame(Prediction)[GetValidationNames()]*100
    return(Prediction)
}

print(system.time(predicted <- predict(TrainingRasters, Model, fun=GBP, index=1:9, progress="text")))

writeRaster(predicted, filename=paste0(OutputDir, "gradientboosting.tif"), progress="text", overwrite=TRUE,
    datatype="INT1U", options=c("COMPRESS=DEFLATE", "ZLEVEL=9", "NUMTHREADS=4"))
