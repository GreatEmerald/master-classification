## Final step: wall-to-wall map
# Choose two models: one with lowest RMSE, one with lowest MAE/highest accuracy
library(ranger)
library(raster)
library(future)
plan("multiprocess")
source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("pixel-based/utils/crossvalidation.r")
source("pixel-based/utils/subpixel-confusion-matrix.r")
source("pixel-based/utils/RFTrain.r")
source("utils/accuracy-statistics.r")

Data.df = LoadTrainingAndCovariates()
Data.df[is.na(Data.df)] = -9999
#Data.sp = Data.df
Data.df = st_set_geometry(Data.df, NULL)
Data.df = AddZeroValueColumns(Data.df)
Data.df = TidyData(Data.df, drop.cols=NULL)
#Data.sp = Data.sp[rownames(Data.df),]

# Validation data
Data.val = LoadValidationAndCovariates()
Data.val[is.na(Data.val)] = -9999
Val.sp = Data.val
Data.val = st_set_geometry(Data.val, NULL)
Data.val = TidyData(Data.val, drop.cols=NULL) # Drops around 1050
Val.sp = Val.sp[rownames(Data.val),]

# Prediction data
Data.pred = LoadPredictionAndCovariates()
Data.pred[is.na(Data.pred)] = -9999

Classes = GetCommonClassNames()
Truth = Data.val[,Classes]

## First, 1-step RF model (lowest RMSE)
FlatPredictionTask = future({RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-walltowall.csv", InflationAdjustment = 0, ValidationSet = as.data.frame(Data.pred))})
resolved(FlatPredictionTask)
FlatPredictions = value(FlatPredictionTask)
head(FlatPredictions)
SpatialPredictions = st_set_geometry(FlatPredictions, Data.pred[["geometry"]])
plot(SpatialPredictions["tree"])
plot(SpatialPredictions["water"])
plot(SpatialPredictions["shrub"])
st_write(SpatialPredictions, "../data/pixel-based/predictions/randomforest-onestep-walltowall.gpkg")

## Make a raster image
SfToRaster = function(sfo, xsamplingrate=0.2, ysamplingrate=0.2, layers=GetCommonClassNames(), fun=max, ...)
{
    xres = (st_bbox(sfo)["xmax"]-st_bbox(sfo)["xmin"])/xsamplingrate
    yres = (st_bbox(sfo)["ymax"]-st_bbox(sfo)["ymin"])/ysamplingrate
    rast = raster()
    sfoextent = extent(sfo)
    sfoextent@xmin = sfoextent@xmin - 0.5*xsamplingrate
    sfoextent@xmax = sfoextent@xmax - 0.5*xsamplingrate
    sfoextent@ymin = sfoextent@ymin - 0.5*ysamplingrate
    sfoextent@ymax = sfoextent@ymax - 0.5*ysamplingrate
    extent(rast) = sfoextent
    ncol(rast) = xres
    nrow(rast) = yres
    PR.ras = rasterize(sfo[layers], rast, fun=fun, ...)
    return(PR.ras)
}

OneStepRaster = SfToRaster(SpatialPredictions) # Note: first layer is id, can be ignored
writeRaster(OneStepRaster, "../data/pixel-based/predictions/randomforest-onestep-walltowall.tif", datatype="INT1U", options=c("COMPRESS=DEFLATE"), overwrite=TRUE)
TIFFtoBMP = function(tifffile, layers=c(5,2,8), prefix="ctw", nas=3)
{
    # BMPs at native resolution (because raster doesn't support PNGs)
    # Set 1: Red: crops, green: trees, blue: water
    # Set 2: Red: bare,  green: grass, blue: shrub
    # Set 3: Red: urban
    InputRaster = brick(tifffile)
    OutputFile = strtrim(tifffile, nchar(tifffile)-4)
    Set = InputRaster[[layers]]
    Set = Set / 100 * 255
    Set[[nas]][is.na(Set[[nas]])] = 255 # Set specified bands to 255
    Set[is.na(Set)] = 0 # Set the rest to 0
    writeRaster(Set, paste0(OutputFile, "-", prefix, ".bmp"), datatype="INT1U", overwrite=TRUE)
}
TIFFtoBMP("../data/pixel-based/predictions/randomforest-onestep-walltowall.tif")
TIFFtoBMP("../data/pixel-based/predictions/randomforest-onestep-walltowall.tif", c(7,4,3), "bgs")
TIFFtoBMP("../data/pixel-based/predictions/randomforest-onestep-walltowall.tif", c(6,6,6), "urban")
TIFFtoBMP("../data/pixel-based/predictions/randomforest-onestep-walltowall.tif", c(3,2,4), "stg")
plotRGB(OneStepRaster, "shrub", "tree", "grassland", stretch="lin")

## Second, 3-step median forest (lowest MAE)
Med3PredictionTask = future({RFTrain3("../data/pixel-based/predictions/", "randomforest-threestep-median-walltowall.csv", PredictType="quantiles", ValidationSet = as.data.frame(Data.pred))})
resolved(Med3PredictionTask)
Med3Prediction = value(Med3PredictionTask)
Med3Prediction = st_set_geometry(Med3Prediction, Data.pred[["geometry"]])
st_write(Med3Prediction, "../data/pixel-based/predictions/randomforest-median-threestep-walltowall.gpkg")

Med3Raster = SfToRaster(Med3Prediction)
writeRaster(Med3Raster, "../data/pixel-based/predictions/randomforest-median-threestep-walltowall.tif", datatype="INT1U", options=c("COMPRESS=DEFLATE"), overwrite=TRUE)
plotRGB(Med3Raster, "shrub", "tree", "grassland", stretch="lin")

TIFFtoBMP("../data/pixel-based/predictions/randomforest-median-threestep-walltowall.tif")
TIFFtoBMP("../data/pixel-based/predictions/randomforest-median-threestep-walltowall.tif", c(3,2,4), "stg")

TIFFtoBMP("../data/pixel-based/predictions/randomforest-median-threestep-walltowall.tif", c(3,2,4), "stg")

# Produce individual images for each class
for (layeridx in 2:nlayers(OneStepRaster))
{
    Class = GetCommonClassNames()[layeridx-1]
    TIFFtoBMP("../data/pixel-based/predictions/randomforest-onestep-walltowall.tif",
              c(layeridx,layeridx,layeridx), Class)
}
for (layeridx in 2:nlayers(OneStepRaster))
{
    Class = GetCommonClassNames()[layeridx-1]
    TIFFtoBMP("../data/pixel-based/predictions/randomforest-median-threestep-walltowall.tif",
              c(layeridx,layeridx,layeridx), Class)
}

# Can convert to PNGs
#BMPs = list.files("../data/pixel-based/predictions/", pattern=glob2rx("*.bmp"), full.names=TRUE)
#library(gdalUtils)
#lapply(BMPs, function(BMP) {gdal_translate(BMP, paste0(strtrim(BMP, nchar(BMP)-4), ".png"), options=c("ZLEVEL=9", "NBITS=1"))})
# Actually they are even larger, so never mind

## Comparison between non-RF models
InterceptModel = Truth
InterceptModel[] = 1/length(Classes)
ggplotBox(list(Intercept = InterceptModel,
        Cubist10C = read.csv("../data/pixel-based/predictions/cubist-committees10.csv")/100,
        LassoLambdaMin = read.csv("../data/pixel-based/predictions/lasso-min-9999.csv", row.names=1)/100,
        FNCScaled = read.csv("../data/pixel-based/predictions/fnc-na0-scaled.csv"),
        SVMMean = read.csv("../data/pixel-based/predictions/svm-scale.csv")/100,
        SVMMedian = read.csv("../data/pixel-based/predictions/svm-median-maxgamma25.csv")/100,
        NNet3Layers = read.csv("../data/pixel-based/predictions/nn-3layers-softmax-adam-0na-nodropout.csv", row.names=1)/100
    ),
    Truth/100, main="Other model comparison", outlier.shape=NA)

## Spatial errors

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
plot(PR.ras)
Val.ras = rasterize(Val.sp[Classes], rast, fun=max)
plot(Val.ras)

plotRGB(Val.ras, "shrub", "tree", "grassland", stretch="lin")
plotRGB(PR.ras, "shrub", "tree", "grassland", stretch="lin")
writeRaster(PR.ras, "../rf-2m-raster.tif")
writeRaster(Val.ras, "../validation-raster.tif")

## Model comparison plot

# Load all the model data we cached

# Intercept
InterceptModel = Truth[,Classes]
InterceptModel[] = 1/length(Classes)*100

LM = read.csv("../data/pixel-based/predictions/lm-all-na0.csv")
PLSR = read.csv("../data/pixel-based/predictions/plsr-55c.csv") # With 55 components instead of all 67; optimal is to use all
Lasso = read.csv("../data/pixel-based/predictions/lasso-lambda0000401.csv") # With a slightly higher lambda than 0
Logistic = read.csv("../data/pixel-based/predictions/logistic-na0.csv")
FNC = read.csv("../data/pixel-based/predictions/fnc-na0.csv")
NN = read.csv("../data/pixel-based/predictions/nn-3layers-softmax-nadam-9999na-nodropout.csv", row.names="X")
Cubist = read.csv("../data/pixel-based/predictions/cubist-committees10.csv")
SVM = read.csv("../data/pixel-based/predictions/svm-median.csv")
MRF = read.csv("../data/pixel-based/predictions/multivarrf-all-fixed.csv")
RFSingle = RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-allcovars-validation.csv", InflationAdjustment = 0)[,Classes]
RFThreeStepMedian = RFTrain3("../data/pixel-based/predictions/", "randomforest-threestep-median.csv", PredictType="quantiles")[,Classes]

devEMF::emf("../output/2020-05-08-model-comparison-bar.emf", width=1272/100, height=634/100)
ggplotBox(list(Intercept = InterceptModel, FNC=FNC, LinearLassoPLSR=LM, Logistic=Logistic, 
               MultivariateRF = MRF,  RFSingle = RFSingle, NN=NN, SVM=SVM, Cubist=Cubist, RF3SMedian=RFThreeStepMedian),
          Truth[,Classes], main="Model comparison", outlier.shape=NA)
dev.off()

devEMF::emf("../output/2020-05-08-model-comparison-line.emf", width=1272/100, height=634/100)
ggplotBoxLines(list(Intercept = InterceptModel, FNC=FNC, LinearLassoPLSR=LM, Logistic=Logistic, 
                    MultivariateRF = MRF,  RFSingle = RFSingle, NN=NN, SVM=SVM, Cubist=Cubist, RF3SMedian=RFThreeStepMedian),
               Truth[,Classes], main="Model comparison")
dev.off()

ggplotBox(list(
    RFSingle = RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-allcovars-validation.csv", InflationAdjustment = 0)[,Classes]/100,
    RFTwoStep = ScalePredictions(RFTrain("../data/pixel-based/predictions/", "randomforest-twostep-truncated-validation.csv", InflationAdjustment = 1, TruncateZeroes = TRUE)[,Classes], FALSE)/100,
    RFThreeStep = RFTrain3("../data/pixel-based/predictions/", "randomforest-threestep-validation.csv")[,Classes]/100,
    RFSingleMedian = ScalePredictions(RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-median-validation.csv", InflationAdjustment = 0, PredictType="quantiles")[,Classes], FALSE)/100,
    RFTwoStepMedian = ScalePredictions(RFTrain("../data/pixel-based/predictions/", "randomforest-twostep-truncated-median.csv", InflationAdjustment = 1, TruncateZeroes = TRUE, PredictType="quantiles")[,Classes], FALSE)/100,
    RFThreeStepMedian = RFTrain3("../data/pixel-based/predictions/", "randomforest-threestep-median.csv", PredictType="quantiles")[,Classes]/100,
    RFOnlyRS = RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-rscovars-validation.csv", InflationAdjustment = 0, covars=RSCovars)[,Classes]/100,
    Intercept = InterceptModel
), Truth[,Classes]/100, main="Random forest model comparison", outlier.shape=NA)


