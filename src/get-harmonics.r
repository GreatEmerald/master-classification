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
VrtFilename = paste0(OutputDir, "harmonics2.vrt")

# Create virtual stack
Vrt = timeVrtProbaV(SemiCleanDir, pattern = BandPattern, vrt_name = VrtFilename, tile = TileOfInterest,
    return_raster = TRUE, te = c(xmin, ymin, xmax, ymax))

TS = timeVrtProbaV(SemiCleanDir, pattern = BandPattern, vrt_name = VrtFilename, tile = TileOfInterest,
    return_raster = FALSE, te = c(xmin, ymin, xmax, ymax))

Bands = TS[TS$date == TS$date[1], 'band']
Dates = TS[TS$band == Bands[1], 'date']

Dates2 = TS[TS$band == Bands[2], 'date']
Dates == Dates2

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

phaser = function(co, si)
{
    tau = 2*pi
    return(atan2(si, co) %% tau)
}
amplituder = function(co, si)
{
    return(sqrt(co^2 + si^2))
}

# Apply phaser and amplituder to our test data
LakeCoeffs = brick(OutputFile)
# min max intercept co si co2 si2 co3 si3 trend (blue, NDVI)
# min max intercept co si co2 si2 trend (blue, NDVI)
#spplot(LakeCoeffs[[20]])
#p1 = phaser(LakeCoeffs[[12]], LakeCoeffs[[13]])
p1 = phaser(LakeCoeffs[[4]], LakeCoeffs[[5]])
#spplot(p1)
#a1 = amplituder(LakeCoeffs[[12]], LakeCoeffs[[13]])
a1 = amplituder(LakeCoeffs[[4]], LakeCoeffs[[5]])
#spplot(a1)
#p2 = phaser(LakeCoeffs[[14]], LakeCoeffs[[15]])
p2 = phaser(LakeCoeffs[[6]], LakeCoeffs[[7]])
#spplot(p2)
#a2 = amplituder(LakeCoeffs[[14]], LakeCoeffs[[15]])
a2 = amplituder(LakeCoeffs[[6]], LakeCoeffs[[7]])
#spplot(a2)
#p3 = phaser(LakeCoeffs[[18]], LakeCoeffs[[19]])
#spplot(p3)
#a3 = amplituder(LakeCoeffs[[18]], LakeCoeffs[[19]])
#spplot(a3)
brick(p1, a1, p2, a2, filename=paste0(OutputDir, "phase-amplitude.tif"), overwrite=TRUE)
