# Clean images using the time series method
library(probaV)
library(tools)
source("utils/set-temp-path.r")
source("../../cloud_filter.r") # JD's extra utility script

IntermediaryDir = "../../userdata/composite/radiometry/"
OutputDir = "../../userdata/composite/tscleaned/"
TileOfInterest = "X20Y01"

# Subset for testing
#xmin <- 27
#xmax <- 28
#ymin <- 58
#ymax <- 59

# Create a virtual raster (faster stack) from all blue bands
VrtPattern = "(BLUE)_sm.tif$"
VrtName = file.path(paste0(OutputDir,TileOfInterest, "_",paste0(VrtPattern, collapse = "_"), ".vrt"))
Vrt = timeVrtProbaV(IntermediaryDir, pattern = VrtPattern, vrt_name = VrtName, tile = TileOfInterest,
    return_raster = TRUE)#,
    #te = c(xmin, ymin, xmax, ymax))

# QC file and log file names
QCFile = paste0(OutputDir, 'cloud_filter.envi')
LogFile = paste0(OutputDir, 'cloud_filter.log')

Cores = 31
psnice(value = min(Cores - 1, 19))
QCMask = cloud_filter(x = Vrt, probav_sm_dir = IntermediaryDir, pattern = VrtPattern,
    tiles = TileOfInterest, minrows = 1, mc.cores = Cores, logfile=LogFile, overwrite=TRUE,
    span=0.3, cf_bands = c(1), thresholds=c(-30, Inf), filename = QCFile)

# select date
which(names(Vrt) == "PROBAV_S5_TOC_X20Y01_20160711_100M_V001_BLUE_sm.tif")
#spplot(Vrt[[124]])
#spplot(QCMask[[124]])
#spplot(Vrt[[128]])
#spplot(QCMask[[128]])
#spplot(Vrt[[140]])
#spplot(QCMask[[140]])
#spplot(Vrt[[132]])
#spplot(QCMask[[132]])

# Apply mask to data
# Basically that means cleaning data all over again, file by file.
