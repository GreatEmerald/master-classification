# Script for filtering temporal outliers and extracting harmonic metrics

library(probaV)
library(pbapply)
#library(foreach)
library(sf)
source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/ProbaVDataDirs.r")

HarmonicsOutputDB = "../data/pixel-based/timeseries/harmonics.gpkg"
InputDB = "../data/pixel-based/timeseries/timeseries-cloudfree.gpkg"

#TileList = GetTileList(LoadGlobalRasterPoints())
Dates = LoadRawDataDirs()$date
ColDates = paste0("X", gsub("-", "", Dates))

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

NDVISF = st_read(InputDB, "NDVI")


GetHarmonics = function(TS)
{
    if (all(is.na(TS)))
    {
        c(min=NA, max=NA, intercept=NA, co=NA, si=NA, co2=NA, si2=NA, trend=NA,
                    phase1=NA, amplitude1=NA, phase2=NA, amplitude2=NA)
    } else {
        HarmCoefs = getHarmMetrics(TS, dates=Dates, order=2)
        p1 = phaser(HarmCoefs["co"], HarmCoefs["si"])
        p2 = phaser(HarmCoefs["co2"], HarmCoefs["si2"])
        a1 = amplituder(HarmCoefs["co"], HarmCoefs["si"])
        a2 = amplituder(HarmCoefs["co2"], HarmCoefs["si2"])
        c(HarmCoefs, phase1=p1, amplitude1=a1, phase2=p2, amplitude2=a2)
    }
}

HarmMetrics = t(pbapply(as.matrix(as.data.frame(NDVISF)[,ColDates]), 1, GetHarmonics))
HarmMetrics = cbind(NDVISF[, c("X", "Y")], HarmMetrics)
names(HarmMetrics)[3:(length(HarmMetrics)-1)] = c(
    "min", "max", "intercept", "co", "si", "co2", "si2", "trend", "phase1", "amplitude1", "phase2", "amplitude2")

st_write(HarmMetrics, HarmonicsOutputDB)
