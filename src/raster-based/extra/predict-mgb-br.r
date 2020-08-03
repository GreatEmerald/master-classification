# Quick test to see how gradient boosting with binary relevance performs

library(xgboost)
library(tools)
library(raster)
source("utils/set-temp-path.r")
source("utils/load-data.r")

Cores = 31
TempDir = "../../userdata/temp/"
OutputDir = "../../userdata/predictions/"

alldata = LoadClassificationData()
TrainingData = alldata[,GetTrainingNames(exclude=c("osavi", "aspect", "is.water", "height"))]
TrainingRasters = LoadTrainingRasters()

GBRP = function(Model, blockvals, ...)
{
    PredRaw = predict(Model, newdata=as.matrix(blockvals), ...)
    return(PredRaw)
}

AllPredictions = stack()
print("Starting prediction phase 1...")
for (Class in GetValidationNames())
{
    print(Class)
    TrainMatrix = xgb.DMatrix(as.matrix(TrainingData@data), label=alldata@data[,Class]/100)
    Model = xgboost(TrainMatrix, nrounds=50, objective="reg:logistic")

    print(system.time(predicted <- predict(TrainingRasters, Model, fun=GBRP,
        filename=paste0(TempDir, Class, ".grd"), progress="text", overwrite=TRUE)))
    
    AllPredictions = stack(AllPredictions, predicted)
}

print("Starting prediction phase 2...")
RasterScaling = function(x)
{
    return(round(x / sum(x) * 100))
}
system.time(ScaledPredictions <- calc(AllPredictions, RasterScaling, progress="text"))
names(ScaledPredictions) = GetValidationNames()
print("Writing...")
ScaledPredictions = writeRaster(ScaledPredictions, filename = paste0(OutputDir, "gbregression-gblinear.tif"),
    datatype="INT1U", options=c("COMPRESS=DEFLATE", "ZLEVEL=9", "NUMTHREADS=4"), overwrite=TRUE)

