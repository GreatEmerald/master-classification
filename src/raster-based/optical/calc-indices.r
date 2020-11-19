# Get OSAVI and LSWI indices from the composited raster
# Then merge everything (and NDVI) into one file
# Also extract the water mask out of the QC
# Can't merge radiometry and indices due to different data type
library(raster); source("utils/set-temp-path.r")
source("utils/GetProbaVQCMask.r")

Dir = "../../userdata/indices-no-na/"
#SemiCleanDir = "../../userdata/semicleaned/"
Radiometry = brick(paste0(Dir, "composite.tif"))
OSAVI = overlay(Radiometry, fun=function(RED, NIR, BLUE, SWIR){return(1.16 * ((NIR-RED)/(NIR+RED+0.16)))},
    datatype="FLT4S", filename=paste0(Dir, "OSAVI.tif"), overwrite=TRUE, progress="text",
    options=c("COMPRESS=DEFLATE", "ZLEVEL=9", "NUMTHREADS=4"))
LSWI = overlay(Radiometry, fun=function(RED, NIR, BLUE, SWIR){return((NIR-SWIR)/(NIR+SWIR))},
    datatype="FLT4S", filename=paste0(Dir, "LSWI.tif"), overwrite=TRUE, progress="text",
    options=c("COMPRESS=DEFLATE", "ZLEVEL=9", "NUMTHREADS=4"))

# Do not add NDVI because it's colinear with OSAVI

# This should be the same as max(NDVIs), but just to make sure follow the same procedure
#NDVIs = stack(list.files(paste0(SemiCleanDir, "ndvi/"), pattern=glob2rx("*NDVI*.tif"), full.names = TRUE))
#SummerNDVIs = subset(NDVIs,
#    which(names(NDVIs) == "PROBAV_S5_TOC_X20Y01_20160601_100M_V001_NDVI_sm"):
#        which(names(NDVIs) == "PROBAV_S5_TOC_X20Y01_20160826_100M_V001_NDVI_sm"))
#MaxNDVI = raster(paste0(Dir, "maxndvi.tif"))
#NDVI = stackSelect(SummerNDVIs, MaxNDVI, datatype="FLT4S", filename=paste0(Dir, "ndvi_composite.tif"),
#    overwrite=TRUE, progress="text")
#Everything = stack(OSAVI, LSWI)
#Everything = writeRaster(Everything, datatype="FLT4S", filename=paste0(Dir, "indices.tif"), overwrite=TRUE,
#    progress="text")
#plotRGB(Everything)

QCExample = raster("/data/MTDA/TIFFDERIVED/PROBAV_L3_S5_TOC_100M/2015/20150616/PROBAV_S5_TOC_20150616_100M_V101/PROBAV_S5_TOC_X20Y01_20150616_100M_V101_SM.tif")
WaterQC = GetProbaVQCMask(land = FALSE)
SelectWater = function(x)
{
    return(x %in% WaterQC)
}
WaterMask = calc(QCExample, fun=SelectWater)
WaterMask = writeRaster(WaterMask, datatype = "INT1U", filename = paste0(Dir, "watermask.tif"),
    options=c("COMPRESS=DEFLATE", "ZLEVEL=9", "NUMTHREADS=4", "NBITS=1"))
