# Script for filtering temporal outliers and extracting harmonic metrics

library(probaV)
library(foreach)
source("pixel-based/utils/load-sampling-data.r")

MaskedOutputDir = "../data/pixel-based/masked-ndvi/"
HarmonicsOutputDir = "../data/pixel-based/covariates/"

TileList = c("X23Y03", "X16Y04")#GetTileList()
Dates = LoadRawDataDirs()$date

TemporalFilter = function(TimeSeriesToMask, TimeSeriesBlue, Dates, span=0.3, threshold=c(-30, Inf), ...)
{
    CloudMask = smoothLoess(TimeSeriesBlue, dates=Dates, res_type="QA", span=span, threshold=threshold, ...)
    TimeSeriesToMask[CloudMask != 1] = NA
    return(TimeSeriesToMask)
}

# TODO: move to utils
phaser = function(co, si)
{
    tau = 2*pi
    return(atan2(si, co) %% tau)
}
amplituder = function(co, si)
{
    return(sqrt(co^2 + si^2))
}

GlobalNDVIHarmonics = foreach(Tile=iter(TileList), .combine="rbind") %do%
{
    # Load time series of blue (for reference) and NDVI (we use that after masking clouds)
    BlueTS = LoadVIMatrix(Tile, "RADIOMETRY", 3)
    NDVITS = LoadVIMatrix(Tile, "NDVI", 1)
    
    NDVIFiltered = foreach(i=1:nrow(BlueTS), .combine="rbind", .inorder=TRUE) %do%
    {
        if (sum(!is.na(BlueTS[i,])) < 7)
        {
            NA
        } else {
            TemporalFilter(NDVITS[i,], BlueTS[i,], Dates)
        }
    }
    rownames(NDVIFiltered) = rownames(NDVITS)
    write.csv(NDVIFiltered, paste0(MaskedOutputDir, Tile, ".csv"))
    
    NDVIHarmonics = foreach(i=1:nrow(NDVIFiltered), .combine="rbind", .inorder = TRUE) %do%
    {
        if (all(is.na(NDVIFiltered[i,])))
        {
            data.frame(min=NA, max=NA, intercept=NA, co=NA, si=NA, co2=NA, si2=NA, trend=NA,
                       phase1=NA, amplitude1=NA, phase2=NA, amplitude2=NA)
        } else {
            HarmCoefs = getHarmMetrics(NDVIFiltered[i,], dates=Dates, order=2)
            p1 = phaser(HarmCoefs["co"], HarmCoefs["si"])
            p2 = phaser(HarmCoefs["co2"], HarmCoefs["si2"])
            a1 = amplituder(HarmCoefs["co"], HarmCoefs["si"])
            a2 = amplituder(HarmCoefs["co2"], HarmCoefs["si2"])
            data.frame(t(HarmCoefs), phase1=p1, amplitude1=a1, phase2=p2, amplitude2=a2)
        }
    }
    rownames(NDVIHarmonics) = rownames(NDVITS)
    NDVIHarmonics
}

write.csv(GlobalNDVIHarmonics, paste0(HarmonicsOutputDir, "harmonics.csv"))

