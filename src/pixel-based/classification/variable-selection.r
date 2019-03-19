# Get uncorrelated covars by dropping the correlated ones
# Input into lasso later on
library(caret)
#library(reshape2)
library(corrr) # Should be in CRAN but had problems, so devtools::install_github("drsimonj/corrr")

source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")

Data.df = as.data.frame(LoadTrainingAndCovariates())
length(GetAllPixelCovars()) # Start: 313
# Automated selection based on correlations
PairwiseCor = cor(Data.df[,GetAllPixelCovars()], use="pairwise.complete.obs")
CorrColumns = findCorrelation(PairwiseCor)
names(Data.df[,GetAllPixelCovars()])[-CorrColumns] # 88 left, but choice is spurious (e.g. no slope)

# Case by case
# Base R is a bit annoying to melt with having to deal with duplicates
#PairedCor = melt(PairwiseCor)
#nrow(PairedCor)
#length(unique(PairedCor$value))*2
#PairedCor[PairedCor$Var1]
#PairwiseCorrr = correlate(Data.df[,GetAllPixelCovars()])
#CorLong = stretch(shave(PairwiseCorrr), TRUE) # Remove a triangle, and diagonals, then make it a long format

# Remove one and recalculate
# Amplitude: a few values way above reasonable limits, log-transformed looks OK
#CorLong[order(abs(CorLong$r), decreasing=TRUE),][1,]
#plot(amplitude1~amplitude2, Data.df, xlim=c(0, 1000), ylim=c(0, 1000))
#plot(log(amplitude1)~log(amplitude2), Data.df)
#Data.df$amplitude1[Data.df$amplitude1 > 100] = NA
#Data.df$amplitude2[Data.df$amplitude2 > 100] = NA
# Decision: log-transform always

RecalcCor = function(covars = GetAllPixelCovars(), ...)
{
    PairwiseCorrr = correlate(Data.df[,covars], ...)
    CorLong = stretch(shave(PairwiseCorrr), TRUE)
    CorLongOrder = as.data.frame(CorLong[order(abs(CorLong$r), decreasing=TRUE),])
    print(CorLongOrder[1,])
    CorSum1 = sum(abs(CorLongOrder[CorLongOrder$x == CorLongOrder[1,1] | CorLongOrder$y == CorLongOrder[1,1],"r"][-1]))
    CorSum2 = sum(abs(CorLongOrder[CorLongOrder$x == CorLongOrder[1,2] | CorLongOrder$y == CorLongOrder[1,2],"r"][-1]))
    print(paste("Correlation sum of", CorLongOrder[1,1], "is", CorSum1))
    print(paste("Correlation sum of", CorLongOrder[1,2], "is", CorSum2))
    print(plot(Data.df[[CorLongOrder[1,1]]], Data.df[[CorLongOrder[1,2]]], xlab=CorLongOrder[1,1], ylab=CorLongOrder[1,2], main=CorLongOrder[1,3]))
    return(CorLongOrder)
}

# start with Pearson to rule out linear correlations, then go for Spearman for the non-linear ones (Kendall is too slow)
CorLong = RecalcCor() # Soil PH
plot(sol_ph.0cm~sol_ph.10cm, Data.df) # True high correlation, pick one
RemainingCovars = GetAllPixelCovars()[!GetAllPixelCovars() %in% "sol_ph.0cm"] # Vegetation not likely to be affected by topsoil, choose 10cm

CorLong = RecalcCor(RemainingCovars) # Soil sand, same
RemainingCovars = RemainingCovars[!RemainingCovars %in% "sol_sand.0cm"]

CorLong = RecalcCor(RemainingCovars) # Soil PH, same
RemainingCovars = RemainingCovars[!RemainingCovars %in% "sol_ph.200cm"]

CorLong = RecalcCor(RemainingCovars) # KCL, deepest layer is not overly useful
RemainingCovars = RemainingCovars[!RemainingCovars %in% "PHIKCL.M.sl7"]

CorLong = RecalcCor(RemainingCovars) # KCL, deepest layer is still not overly useful
RemainingCovars = RemainingCovars[!RemainingCovars %in% "PHIKCL.M.sl4"]

CorLong = RecalcCor(RemainingCovars) ## PHIHOX, hard to tell whether 2 or 3 is better
RemainingCovars = RemainingCovars[!RemainingCovars %in% "PHIHOX.M.sl3"]

CorLong = RecalcCor(RemainingCovars) # Remove 7
RemainingCovars = RemainingCovars[!RemainingCovars %in% "PHIHOX.M.sl7"]

CorLong = RecalcCor(RemainingCovars) ## SLTPPT, hard to choose between level 1 and 2; go with 2 as it's not the very surface
RemainingCovars = RemainingCovars[!RemainingCovars %in% "SLTPPT.M.sl1"]

CorLong = RecalcCor(RemainingCovars) # SNDPPT, hard to choose between level 2 and 3; decided on keeping 2 so keep consistent
RemainingCovars = RemainingCovars[!RemainingCovars %in% "SNDPPT.M.sl3"]

CorLong = RecalcCor(RemainingCovars) # Soil PH, same
RemainingCovars = RemainingCovars[!RemainingCovars %in% "sol_ph.100cm"]

RemainingCovars = RemainingCovars[!RemainingCovars %in% "sol_ph.30cm"] # Consistency also across SoilGrids
RemainingCovars = RemainingCovars[!RemainingCovars %in% "sol_sand.100cm"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "PHIKCL.M.sl6"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "PHIKCL.M.sl3"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "PHIHOX.M.sl6"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "PHIHOX.M.sl1"] # Consistency to choose level 2
plot(wc2.0_30s_tmin_01~wc2.0_30s_tmin_02, Data.df)
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_vapr_02"] ## Water vapour of Jan or Feb. Jan works slightly better and it's actually colder.
RemainingCovars = RemainingCovars[!RemainingCovars %in% "sol_clay.0cm"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "sol_clay.100cm"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "CECSOL.M.sl6"] 
RemainingCovars = RemainingCovars[!RemainingCovars %in% "SLTPPT.M.sl5"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "SLTPPT.M.sl3"] # Choice between 2 and 3
RemainingCovars = RemainingCovars[!RemainingCovars %in% "PHIHOX.M.sl5"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "CLYPPT.M.sl6"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "SNDPPT.M.sl6"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "SNDPPT.M.sl1"] # Same
RemainingCovars = RemainingCovars[!RemainingCovars %in% "sol_bulkdens.200cm"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "sol_sand.30cm"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "PHIHOX.M.sl4"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "CECSOL.M.sl4"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "WWP.M.sl6"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tavg_01"] ## Average temperature of Jan vs Dec. Dec works much better.
RemainingCovars = RemainingCovars[!RemainingCovars %in% "sol_ph.60cm"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "CLYPPT.M.sl3"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "CRFVOL.M.sl1"] # Same
RemainingCovars = RemainingCovars[!RemainingCovars %in% "WWP.M.sl3"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "PHIKCL.M.sl5"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "SLTPPT.M.sl4"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "SNDPPT.M.sl5"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "sol_sand.60cm"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_vapr_07"] # Vapour for 08 or 07, 08 is slightly better
RemainingCovars = RemainingCovars[!RemainingCovars %in% "sol_clay.200cm"] # At this point we're getting meaningful cor differences between paired variables
RemainingCovars = RemainingCovars[!RemainingCovars %in% "CECSOL.M.sl7"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "bio13"] # bio13 vs bio16: precipitation of wettest month or quarter; we have months already, keep quarter; also needs a log-transform
RemainingCovars = RemainingCovars[!RemainingCovars %in% "PHIKCL.M.sl1"] # Level 2 again
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tmin_02"] # Min temperature Jan/Feb, Jan is the coldest month so use that
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_srad_12"] # Solar radiation between Jan and Dec
RemainingCovars = RemainingCovars[!RemainingCovars %in% "SNDPPT.M.sl4"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "WWP.M.sl5"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tmin_07"] # 7 vs 8 and 8 is warmer...
RemainingCovars = RemainingCovars[!RemainingCovars %in% "sol_bulkdens.100cm"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "sol_bulkdens.0cm"]
#RemainingCovars = RemainingCovars[!RemainingCovars %in% "sol_ph.0cm"] # Prefer 1 over 0
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tmax_07"] # Same with tmax
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tavg_07"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "SLTPPT.M.sl7"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tavg_12"] # 12 vs 02: going for colder 02
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tmax_02"] # 02 vs 01: 01 is colder
RemainingCovars = RemainingCovars[!RemainingCovars %in% "sol_clay.60cm"]
Data.df = as.data.frame(LoadTrainingAndCovariates())
CorLong = RecalcCor(RemainingCovars)
RemainingCovars = RemainingCovars[!RemainingCovars %in% "SLTPPT.M.sl6"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "CLYPPT.M.sl2"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "ndvi.75"] # Mean NDVI vs 75th percentile; we're in 98% now
RemainingCovars = RemainingCovars[!RemainingCovars %in% "CLYPPT.M.sl7"]
Data.df = as.data.frame(LoadTrainingAndCovariates())
CorLong = RecalcCor(RemainingCovars) 
RemainingCovars = RemainingCovars[!RemainingCovars %in% "CLYPPT.M.sl5"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_vapr_03"] # Vapour 12 vs 03; we'd want 1 for coldest, 4 for midpoint, so doesn't matter
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_vapr_12"] # Vapour for 01 vs 12
RemainingCovars = RemainingCovars[!RemainingCovars %in% "sol_clay.30cm"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "CECSOL.M.sl5"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "ndvi"] ## NDVI vs OSAVI. Oof. OSAVI is less correlated overall though, and we have mean NDVI
RemainingCovars = RemainingCovars[!RemainingCovars %in% "CECSOL.M.sl3"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "CRFVOL.M.sl3"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "WWP.M.sl4"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_prec_02"] # Precipitation: most in 01/least in 07 in south, most in 08/least in 01 in the north, can keep 01/08 consistently; bioclimatic make more sense
RemainingCovars = RemainingCovars[!RemainingCovars %in% "sol_bulkdens.30cm"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "CRFVOL.M.sl5"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "PREMRG.M.Jan"] # PREMRG from SG covars vs worldclim data
RemainingCovars = RemainingCovars[!RemainingCovars %in% "OCSTHA.M.sd3"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tmax_06"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tavg_02"] ## Average vs max temp in winter. 02 not interesting so get rid of it
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tmax_12"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "PREMRG.M.Sep"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "PREMRG.M.Aug"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "WWP.M.sl1"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tmax_03"] ## 03 vs 11. 04 and 10 are useful so these don't matter
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tavg_04"] ## bio1 vs 04tavg; mean is more meaningful
RemainingCovars = RemainingCovars[!RemainingCovars %in% "bio16"] ## bio12 vs bio16. bio12 is annual so keep that
RemainingCovars = RemainingCovars[!RemainingCovars %in% "osavi"] ## EVI vs OSAVI. EVI is less correlated and more common
RemainingCovars = RemainingCovars[!RemainingCovars %in% "bio6"] ## bio11 vs bio6. bio11 is mean so keep that
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_prec_07"] # we are now at 0.97
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tmax_08"] # average vs max, keep average
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tmin_12"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tmax_11"] # same
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tmin_08"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tmin_03"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tmin_06"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_vapr_11"] # Vapour 04 vs 11
RemainingCovars = RemainingCovars[!RemainingCovars %in% "PREMRG.M.Jun"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "CLYPPT.M.sl4"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "ndvi.25"] # mean NDVI vs 25th percentile
RemainingCovars = RemainingCovars[!RemainingCovars %in% "BLDFIE.M.sl7"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "AWCh3.M.sl2"] ## AWCh2.M.sl2 vs AWCh3.M.sl2, prefer 2 because it's in the middle
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_vapr_09"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tavg_03"] # temperature 11 vs 03, doesn't matter
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tmin_11"] # vs avg
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_srad_06"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tmin_05"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tmax_09"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tmax_10"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "sol_organic.0cm"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "sol_sand.200cm"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "PREMRG.M.Oct"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "PREMRG.M.Mar"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "AWCh3.M.sl3"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tavg_10"] # vs bio1
RemainingCovars = RemainingCovars[!RemainingCovars %in% "AWCh3.M.sl1"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "ORCDRC.M.sl2"] ## OCSTHA vs OCDRC, OCSTHA is easier to interpret
RemainingCovars = RemainingCovars[!RemainingCovars %in% "BLDFIE.M.sl1"] ## water content vs bulk density; the denser the less water. Water has direct influence and density not so much
RemainingCovars = RemainingCovars[!RemainingCovars %in% "intercept"] ## trend vs intercept
RemainingCovars = RemainingCovars[!RemainingCovars %in% "BLDFIE.M.sl2"] # we are now in the 0.96s
RemainingCovars = RemainingCovars[!RemainingCovars %in% "CRFVOL.M.sl7"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "roughness"] ## tri vs roughness, that's max distance vs mean distance, keep mean
RemainingCovars = RemainingCovars[!RemainingCovars %in% "OCSTHA.M.sd6"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_vapr_10"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "BLDFIE.M.sl6"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "OCSTHA.M.sd5"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "CRFVOL.M.sl4"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "ORCDRC.M.sl7"] # Also need to log ORCDRC and OCSTHA
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_vapr_04"] # vapour 04 vs mean
#RemainingCovars = RemainingCovars[!RemainingCovars %in% "SNDPPT.M.sl7"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tmin_09"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tmax_05"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_srad_02"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_srad_11"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "AWCtS.M.sl7"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_vapr_05"] # 05 vs 10
RemainingCovars = RemainingCovars[!RemainingCovars %in% "TEXMHT.M.sl6"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tavg_06"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tavg_06"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "PREMRG.M.Feb"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "AWCh1.M.sl6"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "TMDMOD_2001.M.May"] ## MODIS 2001 vs 2011, we use TS from 2014-2017
RemainingCovars = RemainingCovars[!RemainingCovars %in% "sol_organic.100cm"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "SNDPPT.M.sl7"] # Water vs sand, remove sand
RemainingCovars = RemainingCovars[!RemainingCovars %in% "AWCtS.M.sl6"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "sol_bulkdens.60cm"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "ORCDRC.M.sl3"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "bio5"] ## bio5 vs bio10, bio10 is mean so keep it
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_srad_07"] # 0.95s
RemainingCovars = RemainingCovars[!RemainingCovars %in% "PREMRG.M.Nov"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "mean.wind"] ## Mean wind vs min wind; if we can we should keep min and max both, otherwise mean
RemainingCovars = RemainingCovars[!RemainingCovars %in% "AWCh2.M.sl6"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "PHIHOX.M.sl2"] ## SoilGrids vs LandGIS: keep 10cm
RemainingCovars = RemainingCovars[!RemainingCovars %in% "AWCtS.M.sl3"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "WWP.M.sl7"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "AWCh3.M.sl6"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "bio7"] ## bio4 vs bio7, seasonality vs range; seasonality has a better interpretability
RemainingCovars = RemainingCovars[!RemainingCovars %in% "AWCh3.M.sl4"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "TEXMHT.M.sl1"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "PREMRG.M.Apr"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "swir"] ## red vs swir; lasso sugests that red is the most important, yet swir is less correlated
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tmax_04"] # vs bio1
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_wind_05"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "SNDPPT.M.sl2"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "PREMRG.M.Dec"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "AWCtS.M.sl5"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "bio14"] ## bio14 vs bio17; 14 is month
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_vapr_06"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "ORCDRC.M.sl4"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_wind_06"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "PREMRG.M.May"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "BLDFIE.M.sl3"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "PREMRG.M.Jul"] # 0.94s
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_prec_12"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_wind_07"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "AWCh2.M.sl3"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "BLDFIE.M.sl5"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "TEXMHT.M.sl7"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "AWCh3.M.sl5"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_wind_12"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "bio3"] ## bio4 vs bio3; seasonality is easier to explain
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_wind_03"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "TMDMOD_2011.M.Feb"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tmin_10"] # vs bio1
RemainingCovars = RemainingCovars[!RemainingCovars %in% "AWCh2.M.sl5"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tmin_04"] # vs bio1
RemainingCovars = RemainingCovars[!RemainingCovars %in% "AWCtS.M.sl1"]
#RemainingCovars = RemainingCovars[!RemainingCovars %in% "SNDPPT.M.sl1"] ## Water until wilting vs sand, water is more direct for vegetation
RemainingCovars = RemainingCovars[!RemainingCovars %in% "AWCh1.M.sl5"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "ORCDRC.M.sl6"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "CRFVOL.M.sl6"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_wind_08"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "TEXMHT.M.sl5"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "sol_organic.60cm"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "tri"] ## tri vs slope. The plot is art. tri is harder to interpret. But slope needs a log-transform.
#RemainingCovars = RemainingCovars[!RemainingCovars %in% "AWCh2.M.sl7"]
#RemainingCovars = RemainingCovars[!RemainingCovars %in% "BLDFIE.M.sl1"]
#RemainingCovars = RemainingCovars[!RemainingCovars %in% "PHIKCL.M.sl2"] ## PHIHOX vs PHIKCL, keep H2O
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tavg_09"] # 0.93s
RemainingCovars = RemainingCovars[!RemainingCovars %in% "TEXMHT.M.sl2"] # clay vs texture, keep clay since it's not categorical
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_wind_02"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "AWCh3.M.sl7"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "OCSTHA.M.sd4"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tavg_11"] # vs bio11
RemainingCovars = RemainingCovars[!RemainingCovars %in% "mean.vapr"] # Mean vapour vs min, try to keep min/max
RemainingCovars = RemainingCovars[!RemainingCovars %in% "AWCh1.M.sl2"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tmin_01"] ## tmin vs tmax of Jan; it's either coldest or warmest, so just go with less correlation
RemainingCovars = RemainingCovars[!RemainingCovars %in% "BLDFIE.M.sl4"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "TEXMHT.M.sl3"] # 0.92s
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_wind_11"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "blue"] # red vs blue, we know blue is not as accurate
RemainingCovars = RemainingCovars[!RemainingCovars %in% "TMNMOD_2011.M.May"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "TEXMHT.M.sl4"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "TMDMOD_2001.M.Feb"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_prec_09"] # 06 vs 09, significant correlation difference
RemainingCovars = RemainingCovars[!RemainingCovars %in% "min"] # min vs mean ndvi
RemainingCovars = RemainingCovars[!RemainingCovars %in% "PHIKCL.M.sl2"] ## Soil pH from SoilGrids vs LandGIS. SG has two types, so remove that
RemainingCovars = RemainingCovars[!RemainingCovars %in% "TMNMOD_2011.M.Jul"] # Temperature from MODIS vs WolrdClim
RemainingCovars = RemainingCovars[!RemainingCovars %in% "AWCh2.M.sl4"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "max"] # max vs mean ndvi
RemainingCovars = RemainingCovars[!RemainingCovars %in% "PREMRG.M.Jun"] # 0.91s
RemainingCovars = RemainingCovars[!RemainingCovars %in% "TMDMOD_2001.M.Mar"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "TMDMOD_2001.M.Nov"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "AWCh2.M.sl1"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "CLYPPT.M.sl1"] # clay vs water, keep water
RemainingCovars = RemainingCovars[!RemainingCovars %in% "TMDMOD_2001.M.Apr"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "sol_organic.30cm"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "AWCtS.M.sl4"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "sol_sand.10cm"] ## LandGIS sand vs clay, oh boy. Sand is the middle fraction so remove that
# 0.9 threshold reached
CorLong = RecalcCor(RemainingCovars)
length(RemainingCovars) # 93 covars
tail(CorLong)

# Try correlation for nonlinear relations; kendall is too slow for >100 vars still, so spearman
CorLong = RecalcCor(RemainingCovars, method="spearman")
#RemainingCovars = RemainingCovars[!RemainingCovars %in% "OCSTHA.M.sd2"] ## bulk density vs soil organic carbon; carbon needs a log-transform and is overall more correlated, so eliminate it
RemainingCovars = RemainingCovars[!RemainingCovars %in% "ORCDRC.M.sl1"] # ORCDRC vs ORSTHA again, keep latter
RemainingCovars = RemainingCovars[!RemainingCovars %in% "sol_ph.10cm"] ## Soil pH vs annual precipitation; the more water, the more acidic. Curious. Rain washes away alkali. Precipitation is more important for non-vegetation though.
RemainingCovars = RemainingCovars[!RemainingCovars %in% "bio4"] ## yabs vs bio4, temperature sd... Little seasonality at the equator, high seasonality at middle latitudes (40-ish). Seasonality is harder to explain and is higher correlated with everything else.
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_prec_06"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_wind_09"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "CECSOL.M.sl1"]
RemainingCovars = RemainingCovars[!RemainingCovars %in% "wc2.0_30s_tavg_05"] # can't defeat bio1
CorLong = RecalcCor(RemainingCovars, method="spearman") # 0.9 threshold reached
length(RemainingCovars) # 86
# Next is to either do lasso or stepwise with a logistic regression
toString(RemainingCovars) # This is put into GetUncorrelatedCovars()
