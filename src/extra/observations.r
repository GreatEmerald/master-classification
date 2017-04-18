# Quick test to see if it's possible to get a map of total NDVI observations
library(raster)
library(probaV)
library(tools)
source("utils/set-temp-path.r")

CleanDir = "../../userdata/semicleaned/ndvi/"
OutputDir = "../../userdata/observations/"
TempDir = "../../userdata/temp/"
TileOfInterest = "X20Y01"

BandPattern = "NDVI_sm.tif$"
VrtFilename = paste0(TempDir, "harmonics.vrt")

# Create virtual stack
Vrt = timeVrtProbaV(CleanDir, pattern = BandPattern, vrt_name = VrtFilename, tile = TileOfInterest,
    return_raster = TRUE)

Obs = sum(!is.na(Vrt))
writeRaster(Obs, datatype="INT1U", filename=paste0(OutputDir, "observations-semicleaned.tif"))
