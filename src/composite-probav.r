# PROBA-V images are often cloudy, to the point that no one 5-day composite is ever cloud-free for a single tile.
# Thus we need to composite images manually, starting with the 5-day composites.
# First, bad quality pixels need to be discarded. There should be no ice in summer (even in the middle of Finland), so any such "ice" is actually a cloud.
# Water is a class, keep it. It may be useful as an additional variable.
# Thus discard anything cloudy/shadowed/ice (assert b0-2 == 000), and bad quality (assert b4-7 == 1)
# In dec the good pixels are 240 (water) and 248 (land)
# Some pixels may be covered by not all bands. If there is land cover change between dates, that might result in weird results, so also discard partial pixels like that.
# And the cloud detection algorithm used by PROBA-V is not optimal.
# Thus either get the highest NDVI, or also remove outliers from a LOESS function, or calculate distance from nearest cloud.
# Dealing with bit flags is not something humans should do. So instead have a function that converts a data frame with readable names to bit flags internally.

library(probaV)
library(stringr)
library(foreach)
library(doParallel)
source("utils/set-temp-path.r")
source("utils/GetProbaVQCMask.r")

TileOfInterest = "X20Y01"
QC.vals = GetProbaVQCMask(bluegood=TRUE, redgood=TRUE, nirgood=TRUE, swirgood=TRUE, ice=FALSE, cloud=FALSE, shadow=FALSE)

DataDir = "/data/MTDA/TIFFDERIVED/PROBAV_L3_S5_TOC_100M"
OutputDir = "../../userdata/composite"

# Need a list of all numbered directories
lf = list.files(DataDir)
lf = lf[nchar(lf) == 8]
DataDirs = paste0(DataDir,'/',lf)

# Workaround, should be in the function call
patterns = "NDVI.tif$"
OutputDir = "../../userdata/composite/ndvi"
# glob2rx("*D*I*.tif")
processProbaVbatch(DataDirs, tiles = TileOfInterest, start_date = "2016-06-01", end_date = "2016-08-31",
                  QC_val = QC.vals, outdir = OutputDir,
                  #ncores = (detectCores(all.tests = FALSE, logical = TRUE)-1),
                  overwrite=F)

OutputDir = "../../userdata/composite/radiometry"
patterns = "RADIOMETRY.tif$"
processProbaVbatch(DataDirs, tiles = TileOfInterest, start_date = "2016-06-01", end_date = "2016-08-31",
                  QC_val = QC.vals, outdir = OutputDir,
                  ncores = 3,#(detectCores(all.tests = FALSE, logical = TRUE)-1),
                  overwrite=F)

# Load all cleaned images and produce a maximum NDVI composite using overlay
OutputDir = "../../userdata/composite"
NDVIs = stack(list.files(OutputDir, pattern=glob2rx("*NDVI*.tif"), full.names = TRUE))

# Next, apply additional time-series-based cleaning to NDVI
# Needs clean-timeseries.r to have been run
CleanNDVI = brick(paste0(OutputDir, "/tscleaned/CleanNDVI.tif"))
names(CleanNDVI) = names(NDVIs)
CleanSummerNDVI = subset(CleanNDVI,
    which(names(CleanNDVI) == "PROBAV_S5_TOC_X20Y01_20160601_100M_V101_NDVI_sm"):
        which(names(CleanNDVI) == "PROBAV_S5_TOC_X20Y01_20160826_100M_V101_NDVI_sm"))

# First, get which of the dates has the highest NDVI
# This doesn't work probably due to which.max confustion, so store things in memory instead
# MaxNDVI = calc(CleanSummerNDVI, fun=which.max, datatype="INT2U", filename=paste0(OutputDir, "/maxndvi.tif"),
#    overwrite=TRUE, progress="text")
temp = which.max(CleanSummerNDVI)
MaxNDVI = writeRaster(temp, filename=paste0(OutputDir, "/maxndvi.tif"), datatype="INT2S",
    overwrite=TRUE, progress="text")
rm(temp)

SemiCleanRadDir = "../../userdata/semicleaned/radiometry"
#for (i in 1:length(Bands))
registerDoParallel(cores = 4)
foreach(i=1:length(Bands), .p) %dopar%
{
    Bands = c("BLUE", "RED0", "NIR0", "SWIR")
    Radiometry = stack(list.files(SemiCleanRadDir, pattern=glob2rx(paste0("*", Bands[i], "*.tif")),
        full.names = TRUE))
    SummerRadiometry = subset(Radiometry,
        which(names(Radiometry) == paste0("PROBAV_S5_TOC_X20Y01_20160601_100M_V101_",Bands[i],"_sm")):
        which(names(Radiometry) == paste0("PROBAV_S5_TOC_X20Y01_20160826_100M_V101_",Bands[i],"_sm")))
    stackSelect(SummerRadiometry, MaxNDVI, datatype="INT2S", overwrite=TRUE, progress="text",
        filename=paste0(OutputDir, "/", Bands[i], "_composite.tif"))
}
# Also write a multilayer file (for visualisation etc.)
Composite = writeRaster(stack(paste0(OutputDir, "/RED0_composite.tif"), paste0(OutputDir, "/NIR0_composite.tif"),
    paste0(OutputDir, "/BLUE_composite.tif"), paste0(OutputDir, "/SWIR_composite.tif")), datatype="INT2S",
    filename=paste0(OutputDir, "/composite.tif"), progress="text")
plotRGB(Composite)
