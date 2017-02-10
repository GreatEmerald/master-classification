# Stand-alone script that takes a mask from timeseries-based cleaning and applies it to a given set of files
#
# Inputs: QC mask brick, virtual stack to apply it to
# Output: Cleaned brick of layers
library(raster); source("utils/set-temp-path.r")
library(probaV)
library(foreach)
library(doParallel)
library(iterators)
library(tools)

QCMaskFile = "../../userdata/composite/tscleaned/ts-mask-whole-optimised.tif"
CloudyFileDirectory = "../../userdata/semicleaned/ndvi/"
CloudyFilePattern = "NDVI_sm.tif$"
TileOfInterest = "X20Y01"
CleanDirectory = "../../userdata/cleaned/ndvi/"

CloudyVrt = timeVrtProbaV(CloudyFileDirectory, pattern = CloudyFilePattern,
    vrt_name = tempfile("CloudyVrt", rasterOptions()$tmpdir, ".vrt"),
    tile = TileOfInterest, return_raster = TRUE)
QCMask = brick(QCMaskFile)

if (nlayers(CloudyVrt) != nlayers(QCMask))
    stop("Number of layers does not match!")
LayerNames = names(QCMask) = names(CloudyVrt)

# One thread eats around 840 MB max, so limit to 20 cores for 16 GiB
# Alternatively, set rasterOptions() memory settings
Cores = 20
psnice(value = min(Cores - 1, 19))
registerDoParallel(cores = Cores)
foreach(i=iter(LayerNames), .packages = "raster", .verbose = TRUE, .inorder = FALSE) %dopar%
{
    mask(CloudyVrt[[i]], QCMask[[i]], maskvalue=c(2,0),
        filename=paste0(CleanDirectory, i), datatype="FLT4S", progress="text",
        options=c("COMPRESS=DEFLATE", "ZLEVEL=9"))
}
