# Predict a full tile using neural networks

library(neuralnet)
library(raster)
source("utils/set-temp-path.r")
source("utils/load-data.r")

OutputDir = "../../userdata/predictions/"

AllData = LoadClassificationData()
TrainingData = LoadTrainingData()
TrainingRasters = LoadTrainingRasters(exclude=c("osavi", "amplitude1", "aspect", "blue", "is.water", "amplitude2", "tpi"))
TrainingNames = GetTrainingNames(exclude=c("osavi", "amplitude1", "aspect", "blue", "is.water", "amplitude2", "tpi"))

# The rasters need to be scaled to 0-1, which will take forever, but what can you do
#mins = maxs = c()
#for (i in 1:nlayers(TrainingRasters))
#{
#    mins = c(mins, TrainingRasters[[i]]@data@min)
#    names(mins)[i] = names(TrainingRasters)[i]
#    maxs = c(maxs, TrainingRasters[[i]]@data@max)
#    names(maxs)[i] = names(TrainingRasters)[i]
#}

Formula = paste0(paste0(GetValidationNames(), collapse="+"),"~",paste0(TrainingNames, collapse="+"))

ScaleDataUntransf = AllData@data
ScaleDataUntransf$dominant = NULL
maxs = apply(ScaleDataUntransf, 2, max)
mins = apply(ScaleDataUntransf, 2, min)
ScaleDataUntransf = as.data.frame(scale(ScaleDataUntransf, center = mins, scale = maxs - mins))

rasterOptions(chunksize=1e+08, maxmemory=1e+09)
ScaledRasters = scale(TrainingRasters, center = mins[TrainingNames], scale = maxs[TrainingNames] - mins[TrainingNames])

set.seed(0xfedbeef)
ModelUntransf = neuralnet(Formula, ScaleDataUntransf, 9, lifesign="full", rep=10, threshold=0.05)

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
    #names(PredMinScale) = names(prediction)
    return(PredMinScale)
}

NNP = function(ModelUntransf, ...)
{
    PredictionUntransf = compute(ModelUntransf, ..., rep=which.min(ModelUntransf$result.matrix[1,]))
    return(ScaleNNPrediction(PredictionUntransf$net.result))
}

print(system.time(predicted <- predict(ScaledRasters, ModelUntransf, fun=NNP, index=1:9, progress="text")))

writeRaster(predicted, filename = paste0(OutputDir, "neuralnetwork.tif"), progress="text",
    datatype="INT1U", options=c("COMPRESS=DEFLATE", "ZLEVEL=9", "NUMTHREADS=4"), overwrite=TRUE)
