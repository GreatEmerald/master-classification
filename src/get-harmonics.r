# Calculate time series metrics from all current data
#library(robustbase)
library(probaV)
library(tools)
source("utils/set-temp-path.r")

# create output name on the metrics
OutputDir = "../../userdata/timeseries/"
OutputFile = paste0(OutputDir, "harmonics.envi")
LogFile = paste0(OutputDir, "harmonics.log")
TileOfInterest = "X20Y01"
SemiCleanDir = "../../userdata/semicleaned/"

# Subset for testing
xmin <- 27
xmax <- 28
ymin <- 58
ymax <- 59

BandPattern = "(BLUE|NDVI)_sm.tif$"
VrtFilename = paste0(OutputDir, "harmonics.vrt")

# Create virtual stack
Vrt = timeVrtProbaV(SemiCleanDir, pattern = BandPattern, vrt_name = VrtFilename, tile = TileOfInterest,
    return_raster = TRUE, te = c(xmin, ymin, xmax, ymax))

TS = timeVrtProbaV(SemiCleanDir, pattern = BandPattern, vrt_name = VrtFilename, tile = TileOfInterest,
    return_raster = FALSE)

Bands = TS[TS$date == TS$date[1], 'band']
Dates = TS[TS$band == Bands[1], 'date']

Dates2 = TS[TS$band == Bands[2], 'date']

RowsPerThread = 4
Cores = 30

paste("layers:", nlayers(Vrt), "bands:", paste0(Bands, collapse = " "), "dates:", length(Dates),
    "blocks:", blockSize(Vrt, minrows = RowsPerThread)$n, "cores:", Cores)

# run system time
psnice(value = min(Cores - 1, 19))
# Bug
out_name = OutputFile
Harmonics = system.time(getHarmMetricsSpatial(Vrt, TS, minrows = RowsPerThread, mc.cores = Cores,
    logfile=LogFile, overwrite=TRUE, span=0.3, cf_bands = 1, thresholds=c(-30, Inf),
    filename = OutputFile, order = 2, datatype="FLT4S"))
