# Time "prediction" of a dummy set
# Used as control to find the overhead of raster::predict

library(raster)
source("utils/set-temp-path.r")
source("utils/load-data.r")

TempDir = "../../userdata/temp/"
TrainingRasters = LoadTrainingRasters()

dummypredict = function(model, blockvals, ...)
{
    return(matrix(100/9, nrow = nrow(blockvals), ncol = 9))
}

system.time(predicted <- predict(TrainingRasters, lm("1~1"), fun=dummypredict, index=1:9,
        filename=paste0(TempDir, "dummy.grd"), progress="text", overwrite=TRUE))
