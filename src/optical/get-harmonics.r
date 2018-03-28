# Calculate time series metrics from all current data
#
# Input: Semi-cleaned blue and NDVI files. The process cleans them all over again.
# Output: a multiband file with harmonic parameters

#library(robustbase)
library(probaV)
library(tools)
source("utils/set-temp-path.r")

# create output name on the metrics
if (!exists("OutputDir"))
    OutputDir = "../../userdata/harmonics/"
OutputFile = paste0(OutputDir, "harmonic-coefficients.tif")
LogFile = paste0(OutputDir, "get-harmonics.log")
TileOfInterest = "X20Y01"
if (!exists("CleanNDVIDir"))
    CleanNDVIDir = "../../userdata/cleaned/ndvi/"
CleanDir = "../../userdata/cleaned/"
if (!exists("TempDir"))
    TempDir = "../../userdata/temp/"

# Subset for testing
#xmin <- 27
#xmax <- 28
#ymin <- 58
#ymax <- 59
if (exists("xmin") && exists("xmax") && exists("ymin") && exists("ymax"))
    SubsetExtent = c(xmin, ymin, xmax, ymax)

#BandPattern = "(BLUE|NDVI)_sm.tif$"
#VrtFilename = paste0(OutputDir, "harmonics2.vrt")

BandPattern = "NDVI_sm.tif$"
VrtFilename = paste0(TempDir, "harmonics.vrt")

# Create virtual stack
if (exists("SubsetExtent"))
{
    Vrt = timeVrtProbaV(CleanNDVIDir, pattern = BandPattern, vrt_name = VrtFilename, tile = TileOfInterest,
        return_raster = TRUE, te = SubsetExtent)
} else {
    Vrt = timeVrtProbaV(CleanNDVIDir, pattern = BandPattern, vrt_name = VrtFilename, tile = TileOfInterest,
        return_raster = TRUE)
}

if (exists("SubsetExtent"))
{
    TS = timeVrtProbaV(CleanNDVIDir, pattern = BandPattern, vrt_name = VrtFilename, tile = TileOfInterest,
        return_raster = FALSE, te = SubsetExtent)
} else {
    TS = timeVrtProbaV(CleanNDVIDir, pattern = BandPattern, vrt_name = VrtFilename, tile = TileOfInterest,
        return_raster = FALSE, te = SubsetExtent)
}

if (!exists("RowsPerThread"))
    RowsPerThread = 14
if (!exists("Cores"))
    Cores = 16

print(paste("layers:", nlayers(Vrt), "dates:", "blocks:", blockSize(Vrt, minrows = RowsPerThread)$n, "cores:", Cores))

if (!file.exists(OutputFile))
{
    psnice(value = min(Cores - 1, 19))
    print(system.time(Coeffs <- getHarmMetricsSpatial(Vrt, TS, minrows = RowsPerThread, mc.cores = Cores,
        logfile=LogFile, overwrite=TRUE, filename = OutputFile, order = 2, datatype="FLT4S", progress="text")))
} else
    Coeffs = brick(OutputFile)

phaser = function(co, si)
{
    tau = 2*pi
    return(atan2(si, co) %% tau)
}
amplituder = function(co, si)
{
    return(sqrt(co^2 + si^2))
}

# Calculate phase and amplitude from our data
# Parameters, in order:
# min max intercept co si co2 si2 co3 si3 trend (blue, NDVI)
# min max intercept co si co2 si2 trend (blue, NDVI)
p1 = overlay(Coeffs[[4]], Coeffs[[5]], fun=phaser)
a1 = overlay(Coeffs[[4]], Coeffs[[5]], fun=amplituder)
p2 = overlay(Coeffs[[6]], Coeffs[[7]], fun=phaser)
a2 = overlay(Coeffs[[6]], Coeffs[[7]], fun=amplituder)

# Get mean NDVI value
# Could probably try to get it from trend and intercept, but rounding errors might be severe
MeanNDVI = mean(Vrt, na.rm=TRUE)

# Create final output
FinalStack = stack(MeanNDVI, p1, a1, p2, a2)
brick(FinalStack, filename=paste0(OutputDir, "phase-amplitude.tif"), datatype="FLT4S", overwrite=TRUE,
    progress="text", options=c("COMPRESS=DEFLATE", "ZLEVEL=9", "NUMTHREADS=4"))
