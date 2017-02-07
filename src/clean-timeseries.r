# Clean images using the time series method
#
# Input: blue band images.
# Output: a mask file (QCFile), and it applied to NDVI to get an NDVI time series (CleanNDVI)

library(probaV)
library(tools)
source("utils/set-temp-path.r")
source("../../cloud_filter.r") # JD's extra utility script

IntermediaryDir = "../../userdata/semicleaned/radiometry/"
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
file.remove(VrtName)
Vrt = timeVrtProbaV(IntermediaryDir, pattern = VrtPattern, vrt_name = VrtName, tile = TileOfInterest,
    return_raster = TRUE)#,
    #te = c(xmin, ymin, xmax, ymax))

# QC file and log file names
QCFile = paste0(OutputDir, 'ts-mask-whole.tif')
LogFile = paste0(OutputDir, 'ts-cleaning.log')

Cores = 16
Rows = 12
psnice(value = min(Cores - 1, 19))
system.time(
    QCMask <- cloud_filter(x = Vrt, probav_sm_dir = IntermediaryDir, pattern = VrtPattern,
        tiles = TileOfInterest, minrows = Rows, mc.cores = Cores, logfile=LogFile, overwrite=TRUE,
        span=0.3, cf_bands = c(1), thresholds=c(-30, Inf), filename = QCFile)
)

# select date
which(names(Vrt) == "PROBAV_S5_TOC_X20Y01_20160711_100M_V101_BLUE_sm.tif")
#spplot(Vrt[[124]])
#spplot(QCMask[[124]])
#spplot(Vrt[[128]])
#spplot(QCMask[[128]])
#spplot(Vrt[[140]])
#spplot(QCMask[[140]])
#spplot(Vrt[[132]])
#spplot(QCMask[[132]])

# Apply mask to data: NDVI
NDVIDir = "../../userdata/semicleaned/ndvi"
NDVIs = stack(list.files(NDVIDir, pattern=glob2rx("*NDVI*.tif"), full.names = TRUE))
TSCleanDir = "../../userdata/composite/tscleaned/"
QCMask = brick(QCFile)
CleanNDVI = mask(NDVIs, QCMask, maskvalue=c(2,0),
    filename=paste0(TSCleanDir, "CleanNDVI.tif"), datatype="FLT4S", progress="text",
    overwrite=TRUE, options=c("COMPRESS=DEFLATE", "ZLEVEL=9", "NUMTHREADS=30"))

