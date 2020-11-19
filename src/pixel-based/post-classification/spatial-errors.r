# Plot errors spatially or output spatial error files

library(raster)
source("pixel-based/utils/RFTrain.r")
source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("pixel-based/utils/crossvalidation.r")

Data.val = LoadValidationAndCovariates()
Data.val[is.na(Data.val)] = -9999
Val.sp = Data.val
Data.val = st_set_geometry(Data.val, NULL)
Data.val = TidyData(Data.val, drop.cols=NULL) # Drops around 1050
Val.sp = Val.sp[rownames(Data.val),]

Classes = GetCommonClassNames()
Truth = Data.val[,Classes]

# Points
RFSingle = RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-allcovars-validation.csv", InflationAdjustment = 0)
RF3step = RFTrain3("../data/pixel-based/predictions/", "randomforest-threestep-median.csv", PredictType="quantiles")

# 1-step model

plot(Val.sp["shrub"])
PR.sp = st_set_geometry(RFSingle, Val.sp[["geometry"]])
plot(PR.sp["shrub"])

# Errors
RFSingleErr = RFSingle - Data.val[,Classes]
Err.sp = st_set_geometry(RFSingleErr, Val.sp[["geometry"]])
plot(Err.sp["shrub"])
AbsErr.sp = Err.sp
AbsErr.sp = abs(st_set_geometry(AbsErr.sp, NULL))
AbsErr.sp = st_set_geometry(AbsErr.sp, Err.sp[["geometry"]])
plot(AbsErr.sp["shrub"])
st_write(AbsErr.sp, "../output/2020-04-28-rf1-AE.gpkg")

EClusters = st_read("../data/pixel-based/biomes/ProbaV_UTM_LC100_biome_clusters_V3_global.gpkg")
MAE.sp = aggregate(AbsErr.sp, EClusters["bc_id"], mean)
plot(MAE.sp)
st_write(MAE.sp, "../output/2020-04-28-rf1-EC-MAE.gpkg")

# Raster
AbsErr.ras = SfToRaster(AbsErr.sp, 1, 1, fun=mean)
plot(AbsErr.ras)

writeRaster(AbsErr.ras[[-1]], "../output/rf1st-abserr-1deg-mean.tif", datatype="INT1U")

# 3-step model

RF3stepErr = RF3step - Data.val[,Classes]
Err.sp = st_set_geometry(RF3stepErr, Val.sp[["geometry"]])
plot(Err.sp["shrub"])
AbsErr.sp = Err.sp
AbsErr.sp = abs(st_set_geometry(AbsErr.sp, NULL))
AbsErr.sp = st_set_geometry(AbsErr.sp, Err.sp[["geometry"]])
plot(AbsErr.sp["shrub"])
st_write(AbsErr.sp, "../output/2020-10-12-rf3-AE.gpkg")

AbsErr.ras = SfToRaster(AbsErr.sp, 1, 1, fun=mean)
plot(AbsErr.ras)

writeRaster(AbsErr.ras[[-1]], "../output/rf3st-abserr-1deg-mean.tif", datatype="INT1U")

AbsErrOverall = apply(st_set_geometry(AbsErr.sp, NULL), 1, mean)
AbsErrOverall = st_set_geometry(data.frame(Overall = AbsErrOverall), AbsErr.sp$geometry)
st_write(AbsErrOverall, "../output/2020-10-12-rf3-AE-overall.gpkg")
AbsErrOverall.ras = SfToRaster(AbsErrOverall, 1, 1, fun=mean, layers="Overall")
writeRaster(AbsErrOverall.ras[[-1]], "../output/rf3st-abserr-1deg-mean-overall.tif", datatype="INT1U")
