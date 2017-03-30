# Time "prediction" of a dummy set
# Used as control to find the overhead of raster::predict
# Also the RMSE values

library(raster)
source("utils/set-temp-path.r")
source("utils/load-data.r")
source("utils/accuracy-statistics.r")

TempDir = "../../userdata/temp/"
TrainingRasters = LoadTrainingRasters()
AllData = LoadClassificationData()
ValData = LoadValidationData()

# A 4-fold CV for static prediction is always the same
DummyData = ValData@data
DummyData[] = 100.0/9.0

AST = AccuracyStatTable(DummyData, ValData@data)
write.csv(AST, "../data/stat-dummy.csv")
write.csv(CalcErrors(DummyData, ValData@data), "../data/errors-stat-dummy.csv")

# For the pure-nonpure case
DummyData = ValData@data[!AllData@data$pure,]
DummyData[] = 100.0/9.0

AST = AccuracyStatTable(DummyData, ValData@data[!AllData@data$pure,])
write.csv(AST, "../data/stat-dummy-pure.csv")

dummypredict = function(model, blockvals, ...)
{
    return(matrix(100/9, nrow = nrow(blockvals), ncol = 9))
}

system.time(predicted <- predict(TrainingRasters, lm("1~1"), fun=dummypredict, index=1:9,
        filename=paste0(TempDir, "dummy.grd"), progress="text", overwrite=TRUE))
