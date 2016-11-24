# Get OSAVI and LSWI indices from the composited raster
# Then merge everything (and NDVI) into one file
# Also extract the water mask out of the QC
# Can't merge radiometry and indices due to different data type
library(raster); source("utils/set-temp-path.r")
source("utils/GetProbaVQCMask.r")

Dir = "../../userdata/composite/"
Radiometry = brick(paste0(Dir, "radiometry/composite.tif"))
OSAVI = overlay(Radiometry, fun=function(RED, NIR, BLUE, SWIR){return(1.16 * ((NIR-RED)/(NIR+RED+0.16)))},
    datatype="FLT4S", filename=paste0(Dir, "OSAVI.tif"), overwrite=TRUE)
LSWI = overlay(Radiometry, fun=function(RED, NIR, BLUE, SWIR){return((NIR-SWIR)/(NIR+SWIR))},
    datatype="FLT4S", filename=paste0(Dir, "LSWI.tif"), overwrite=TRUE)
# This should be the same as max(NDVIs), but just to make sure follow the same procedure
NDVIs = stack(list.files(paste0(Dir, "ndvi/"), pattern=glob2rx("*NDVI*.tif"), full.names = TRUE))
MaxNDVI = raster(paste0(Dir, "ndvi/maxndvi.tif"))
NDVI = stackSelect(NDVIs, MaxNDVI, datatype="FLT4S", filename=paste0(Dir, "ndvi/ndvi_composite.tif"))

Everything = stack(NDVI, OSAVI, LSWI)
Everything = writeRaster(Everything, datatype="FLT4S", filename=paste0(Dir, "indices.tif"), overwrite=TRUE)
plotRGB(Everything)

QCExample = raster("/data/MTDA/TIFFDERIVED/PROBAV_L3_S5_TOC_100M/2015/20150616/PROBAV_S5_TOC_20150616_100M_V101/PROBAV_S5_TOC_X20Y01_20150616_100M_V101_SM.tif")
WaterQC = GetProbaVQCMask(land = FALSE)
SelectWater = function(x)
{
    if (x %in% WaterQC)
        return(TRUE)
    return(FALSE)
}
WaterMask = calc(QCExample, fun=SelectWater)
WaterMask = writeRaster(WaterMask, datatype = "INT1U", filename = paste0(Dir, "watermask.tif"))
