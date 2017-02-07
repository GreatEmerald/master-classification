# Clean PROBA-V files using built-in cloud mask from QC layer.
#
# Inputs: raw NDVI and radiometry from PROBA-V.
#     Paths found automatically on the VITO VM. Can be rerun to catch new files.
# Output: NDVI and radiometry files with NAs instead of values of bad pixels.

library(probaV)
library(tools)
source("utils/set-temp-path.r")
source("utils/GetProbaVQCMask.r")

TileOfInterest = "X20Y01"
QC.vals = GetProbaVQCMask(bluegood=TRUE, redgood=TRUE, nirgood=TRUE, swirgood=TRUE,
    ice=FALSE, cloud=FALSE, shadow=FALSE)
DataDir = "/data/MTDA/TIFFDERIVED/PROBAV_L3_S5_TOC_100M"
NDVIOutputDir = "../../userdata/semicleaned/ndvi"
RadiometryOutputDir = "../../userdata/semicleaned/radiometry"

# Need a list of all YYYYMMDD numbered directories; these are Collection 0
lf = list.files(DataDir)
lf = lf[nchar(lf) == 8]
# Remove empty directories
Col0Dirs = character()
for (dir in lf)
{
    lsf = list.files(paste0(DataDir, "/", dir))
    if (!identical(lsf, character(0)))
        Col0Dirs = c(Col0Dirs, dir)
}
DataDirs = paste0(DataDir,'/',Col0Dirs)

# Also process Collection 1, YYYY/YYYYMMDD
lf = list.files(DataDir)
lf = lf[nchar(lf) == 4]
Col1Dirs = character()
for (dir in lf)
{
    lsf = list.files(paste0(DataDir, "/", dir))
    lsf = lsf[nchar(lsf) == 8]
    Col1Dirs = c(Col1Dirs, paste0(dir,'/',lsf))
}
DataDirs = c(DataDirs, paste0(DataDir,'/',Col1Dirs))

psnice(value = 3)
# First process radiometry (it can then be used for further cleaning of Collection 0)
processProbaVbatch(DataDirs, tiles = TileOfInterest, QC_val = QC.vals, overwrite=FALSE,
    pattern = "RADIOMETRY.tif$", outdir = RadiometryOutputDir, ncores = 4)

# Then process NDVI (for use in time series)
processProbaVbatch(DataDirs, tiles = TileOfInterest, QC_val = QC.vals, overwrite=FALSE,
    pattern = "NDVI.tif$", outdir = NDVIOutputDir, ncores = 4)
