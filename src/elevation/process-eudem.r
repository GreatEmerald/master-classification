# Process EUDEM, the European Environment Agency DEM
# It has some gaps, so this depends on GLSDEM from get-dem.r to be there too


library(raster)
source("utils/raster-utils.r")
source("utils/dem-statistics.r")

# Each PROBA-V tile is exactly four EUDEM tiles.
# Downloaded from http://www.eea.europa.eu/data-and-maps/data/eu-dem#tab-original-data (no API as far as I know)
StorageDir = "../../userdata/dem/eeadem"
DEMTiles = list.files(StorageDir, pattern = glob2rx("*.tif"), full.names = TRUE)

dems = list()
for(i in 1:length(DEMTiles))
{
    dems[[i]] = raster(DEMTiles[i])
}

# Mosaic first
DEMMosaic = mosaic(dems, fun=mean, filename=paste0(StorageDir, "/mosaic.grd"))
# Calculate derived statistics
PVExample = raster("/data/MTDA/TIFFDERIVED/PROBAV_L3_S5_TOC_100M/20160711/PROBAV_S5_TOC_20160711_100M_V001/PROBAV_S5_TOC_X20Y01_20160711_100M_V001_NDVI.tif")
CalculateDEMStatistics(DEMMosaic, StorageDir, PVExample)
