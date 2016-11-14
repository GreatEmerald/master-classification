# Get OSAVI and LSWI indices from the composited raster
# Then merge everything (and NDVI) into one file
# Can't merge radiometry and indices due to different data type
library(raster)

Dir = "../../userdata/composite/"
Radiometry = raster(paste0(Dir, "radiometry/composite.tif"))
# 1.16 * ((NIR-RED)/(NIR+RED+0.16))
#OSAVI = calc(Radiometry, fun=function(x){1.16 * ((x[2]-x[3])/(x[2]+x[3]+0.16))},
OSAVI = overlay(Radiometry, fun=function(RED, NIR, BLUE, SWIR){return(1.16 * ((NIR-RED)/(NIR+RED+0.16)))},
    datatype="FLT4S", filename=paste0(Dir, "OSAVI.tif"), overwrite=TRUE)
# NIR-SWIR/NIR+SWIR
#LSWI = calc(Radiometry, fun=function(x){(x[2]-x[4])/(x[2]+x[4])},
LSWI = overlay(Radiometry, fun=function(RED, NIR, BLUE, SWIR){return((NIR-SWIR)/(NIR+SWIR))},
    datatype="FLT4S", filename=paste0(Dir, "LSWI.tif"), overwrite=TRUE)
# This should be the same as max(NDVIs), but just to make sure follow the same procedure
NDVIs = stack(list.files(paste0(Dir, "ndvi/"), pattern=glob2rx("*NDVI*.tif"), full.names = TRUE))
MaxNDVI = raster(paste0(Dir, "ndvi/maxndvi.tif"))
NDVI = stackSelect(NDVIs, MaxNDVI, datatype="FLT4S", filename=paste0(Dir, "ndvi/ndvi_composite.tif"))

Everything = stack(NDVI, OSAVI, LSWI)
Everything = writeRaster(Everything, datatype="FLT4S", filename=paste0(Dir, "indices.tif"), overwrite=TRUE)
