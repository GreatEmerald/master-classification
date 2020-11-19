# Make a full-raster prediction using Random Forest

library(ranger)
library(tools)
library(raster)
source("utils/set-temp-path.r")
source("utils/load-data.r")

if (!exists("Cores"))
    Cores = 31
if (!exists("TempDir"))
    TempDir = "../../userdata/temp/"
if (!exists("OutputDir"))
    OutputDir = "../../userdata/predictions/"

if (!exists("Extent"))
    Extent = NULL
if (!exists("Exclude"))
    Exclude = c("osavi", "aspect", "is.water", "height")
if (exists("CovariateDir"))
{
    TrainingRasters = LoadTrainingRasters(extent=Extent, basedir=CovariateDir)
} else {
    TrainingRasters = LoadTrainingRasters(extent=Extent)
}
if (!exists("ClassesToPredict"))
{
    ClassesToPredict = GetValidationNames()
}
if (!exists("Alpha"))
    Alpha = 0.9
if (!exists("MinProp"))
    MinProp = 0.11

TrainingNames = GetTrainingNames(exclude=Exclude)
alldata = LoadClassificationData()

RP = function(...)
{
    psnice(value = min(Cores - 1, 19))
    rp = predict(...)
    return(rp$predictions)
}

FullFormula = paste0("~", paste(TrainingNames, collapse = "+"))
AllPredictions = stack()
print("Starting prediction phase 1...")
for (Class in ClassesToPredict)
{
    print(Class)
    Formula = update.formula(FullFormula, paste0(Class, " ~ ."))
    if (exists("GetVariableImportance") && GetVariableImportance)
    {
        rfmodel = ranger(Formula, alldata@data, splitrule="maxstat", alpha=Alpha, minprop=MinProp, importance="permutation", seed=0xbadcafe)
        print(rfmodel$prediction.error)
        print(importance(rfmodel))
        barplot(importance(rfmodel), main=Class)
    } else {
        rfmodel = ranger(Formula, alldata@data, splitrule="maxstat", alpha=Alpha, minprop=MinProp, seed=0xbadcafe)
    }

    print(system.time(predicted <- predict(TrainingRasters, rfmodel, fun=RP, num.threads=Cores,
        filename=paste0(TempDir, Class, ".grd"), progress="text", overwrite=TRUE, seed=0xbadcafe)))
    
    AllPredictions = stack(AllPredictions, predicted)
}

# Only scale the predictions to 100% if we have all classes
if (length(ClassesToPredict) >= 9)
{
    print("Starting prediction phase 2...")
    RasterScaling = function(x)
    {
        return(round(x / sum(x) * 100))
    }
    system.time(ScaledPredictions <- calc(AllPredictions, RasterScaling, progress="text"))
    names(ScaledPredictions) = GetValidationNames()
    print("Writing...")
    ScaledPredictions = writeRaster(ScaledPredictions, filename = paste0(OutputDir, "randomforest.tif"),
        datatype="INT1U", options=c("COMPRESS=DEFLATE", "ZLEVEL=9", "NUMTHREADS=4"), overwrite=TRUE)
    hdr(ScaledPredictions, "VRT")
} else {
    writeRaster(AllPredictions, filename = paste0(OutputDir, "randomforest.tif"),
        datatype="INT1U", options=c("COMPRESS=DEFLATE", "ZLEVEL=9", "NUMTHREADS=4"), overwrite=TRUE)
}
