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
