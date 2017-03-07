# Make a full-raster prediction using Random Forest

library(ranger)
library(tools)
library(raster)
source("utils/set-temp-path.r")
source("utils/load-data.r")

Cores = 31
TempDir = "../../userdata/temp/"
OutputDir = "../../userdata/predictions/"

TrainingRasters = LoadTrainingRasters()
TrainingNames = GetTrainingNames(exclude=c("osavi", "aspect", "is.water", "height"))
alldata = LoadClassificationData()

RP = function(...)
{
    psnice(value = min(Cores - 1, 19))
    rp = predict(...)
    return(rp$predictions)
}

dummypredict = function(model, blockvals, ...)
{
    return(rep(100/9, nrow(blockvals)))
}

FullFormula = paste0("~", paste(TrainingNames, collapse = "+"))
AllPredictions = stack()
print("Starting prediction phase 1...")
for (Class in GetValidationNames())
{
    print(Class)
    Formula = update.formula(FullFormula, paste0(Class, " ~ ."))
    rfmodel = ranger(Formula, alldata@data, splitrule="maxstat", alpha=0.9, minprop=0.11, seed=0xbadcafe)

    print(system.time(predicted <- predict(TrainingRasters, rfmodel, fun=RP, num.threads=Cores,
        filename=paste0(TempDir, Class, ".grd"), progress="text", overwrite=TRUE, seed=0xbadcafe)))
    
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
ScaledPredictions = writeRaster(ScaledPredictions, filename = paste0(OutputDir, "randomforest2.tif"),
    datatype="INT1U", options=c("COMPRESS=DEFLATE", "ZLEVEL=9", "NUMTHREADS=4"), overwrite=TRUE)
hdr(ScaledPredictions, "VRT")
