# Predict using c-means classification

library(GSIF)
library(Hmisc)
library(foreach)
library(doParallel)
library(tools)
library(raster)
source("utils/set-temp-path.r")
source("utils/load-data.r")

OutputDir = "../../userdata/predictions/"

# Needs a SpatialPixelsDataFrame out of the rasters
Rasters = LoadTrainingRasters(exclude = c("is.water", "amplitude1", "blue", "amplitude2", "tpi", "red",
    "lswi", "aspect", "swir"))
#TrainingPixels = LoadTrainingPixels(exclude = "is.water")
alldata = LoadClassificationData()

GetClassMeans = function(samples = 1:nrow(alldata), validation.idx = 1:9, training.idx = 13:28)
{
    combos = expand.grid(validation=validation.idx, training=training.idx)
    ClassMeans = function(x)
    {
        wtd.mean(alldata@data[samples,x["training"]], alldata@data[samples,x["validation"]])
    }
    cm = apply(combos, 1, ClassMeans)
    c.means = matrix(cm, nrow=length(validation.idx),
        dimnames=list(names(alldata)[validation.idx], names(alldata)[training.idx]))
    return(c.means)
}

GetClassSDs = function(samples = 1:nrow(alldata), validation.idx = 1:9, training.idx = 13:28)
{
    combos = expand.grid(validation=validation.idx, training=training.idx)
    ClassSDs = function(x)
    {
        sqrt(wtd.var(alldata@data[samples,x["training"]], alldata@data[samples,x["validation"]],
            normwt = TRUE))
    }
    csd = apply(combos, 1, ClassSDs)
    c.sds = matrix(csd, nrow=length(validation.idx),
        dimnames=list(names(alldata)[validation.idx], names(alldata)[training.idx]))
    return(c.sds)
}

TestFormula = formula("dominant~nir + osavi + height + slope + mean.ndvi + phase1 + phase2")

Threads = 2
registerDoParallel(cores = Threads)
outputs = foreach(i=0:24, .combine = rbind, .inorder = FALSE, .packages = c("raster", "GSIF"),
    .verbose=TRUE) %dopar%
{
    psnice(value = min(Threads - 1, 19))
    
    # Crop rasters to one tenth: 15 layers use 2.3 GB which means max 7 threads
    
    RasterExtent = extent(Rasters)
    x = i %% 5 * 2
    y = i %/% 5 * 2
    RasterExtent@xmin = RasterExtent@xmin+x
    RasterExtent@ymin = RasterExtent@ymin+y
    RasterExtent@xmax = RasterExtent@xmin+2
    RasterExtent@ymax = RasterExtent@ymin+2
    CroppedRaster = crop(Rasters, RasterExtent)
    
    ValidationPixels = as(CroppedRaster, "SpatialPixelsDataFrame") # This will eat a lot of memory
    
    timer = system.time(cmeans <- spfkm(TestFormula, alldata, ValidationPixels,
        class.c = GetClassMeans(), class.sd = GetClassSDs(), fuzzy.e=1.5))
    
    output = brick(cmeans@mu)
    output = output * 100
    writeRaster(output, filename = paste0(OutputDir, "cmeans", i ,".tif"),
        datatype="INT1U", progress="text", overwrite=TRUE)
    timer
}
print(outputs)
#output = writeRaster(output, filename = paste0(OutputDir, "cmeans1.tif"), datatype="INT1U")#,
#    options=c("COMPRESS=DEFLATE", "ZLEVEL=9", "NUMTHREADS=4"))
#names(output) = names(cmeans@mu)
