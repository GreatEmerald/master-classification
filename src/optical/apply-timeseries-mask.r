# Stand-alone script that takes a mask from timeseries-based cleaning and applies it to a given set of files
#
# Inputs: QC mask brick, virtual stack to apply it to
# Output: Cleaned brick of layers
library(raster); source("utils/set-temp-path.r")
library(probaV)
library(foreach)
library(doParallel)
library(tools)

QCMaskFile = "../../userdata/composite/tscleaned/ts-mask-whole-optimised.tif"
CloudyFileDirectory = "../../userdata/semicleaned/ndvi/"
CloudyFilePattern = "NDVI_sm.tif$"
TileOfInterest = "X20Y01"
CleanFile = "../../userdata/composite/tscleaned/CleanNDVI.tif"

CloudyVrt = timeVrtProbaV(CloudyFileDirectory, pattern = CloudyFilePattern,
    vrt_name = tempfile("CloudyVrt", rasterOptions()$tmpdir, ".vrt"),
    tile = TileOfInterest, return_raster = TRUE)
QCMask = brick(QCMaskFile)

if (nlayers(CloudyVrt) != nlayers(QCMask))
    stop("Number of layers does not match!")
LayerNames = names(CloudyVrt)

Cores = 31
psnice(value = min(Cores - 1, 19))
registerDoParallel(cores = Cores)
LoopCount = nlayers(CloudyVrt)
CleanStack = foreach(i=1:LoopCount, .packages = "raster", .verbose = TRUE, .combine = stack, .multicombine = TRUE) %dopar%
{
    rasterOptions(format="GTiff", datatype="FLT4S", progress="text", timer=TRUE)
    mask(CloudyVrt[[i]], QCMask[[i]], maskvalue=c(2,0))
}
writeRaster(CleanStack, filename=CleanFile, datatype="FLT4S", progress="text",
    overwrite=TRUE, options=c("COMPRESS=DEFLATE", "ZLEVEL=9", "NUMTHREADS=4"))
