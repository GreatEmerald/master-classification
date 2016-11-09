# Merge EU DEM with GLSDEM, preferring EU DEM
library(raster)

GLSDir = "../../userdata/dem/glsdem"
EUDir = "../../userdata/dem/eeadem"
MergedDir = "../../userdata/dem/merged"
files = c("pv-height.tif", "pv-aspect.tif", "pv-tpi.tif", "pv-slope.tif")
GLSFiles = paste0(GLSDir, "/", files)
EUFiles = paste0(EUDir, "/", files)
MergedFiles = paste0(MergedDir, "/", files)

for (i in 1:length(files))
    raster::merge(raster(EUFiles[i]), raster(GLSFiles[i]), filename=MergedFiles[i], overwrite=TRUE)
