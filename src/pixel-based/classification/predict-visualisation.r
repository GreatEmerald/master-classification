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

OneStepRaster = SfToRaster(SpatialPredictions)
writeRaster(OneStepRaster, "../data/pixel-based/predictions/randomforest-onestep-walltowall.tif", datatype="INT1U", options=c("COMPRESS=DEFLATE"), overwrite=TRUE)
TIFFtoBMP = function(tifffile)
{
    # BMPs at native resolution (because raster doesn't support PNGs)
    # Set 1: Red: crops, green: trees, blue: water
    # Set 2: Red: bare,  green: grass, blue: shrub
    # Set 3: Red: urban
    InputRaster = brick(tifffile)
    OutputFile = strtrim(tifffile, nchar(tifffile)-4)
    Set1 = InputRaster[[c(5, 2, 8)]]
    Set1 = Set1 / 100 * 255
    writeRaster(Set1, paste0(OutputFile, "-ctw.bmp"), datatype="INT1U", overwrite=TRUE)
    Set2 = InputRaster[[c(7, 4, 3)]]
    Set2 = Set2 / 100 * 255
    writeRaster(Set2, paste0(OutputFile, "-bgs.bmp"), datatype="INT1U", overwrite=TRUE)
    Urban = InputRaster[[c(6, 6, 6)]]
    Urban[[2]][!is.na(Urban[[2]])] = 0
    Urban[[3]][!is.na(Urban[[3]])] = 0
    Urban = Urban / 100 * 255
    writeRaster(Urban, paste0(OutputFile, "-urban.bmp"), datatype="INT1U", overwrite=TRUE)
}
TIFFtoBMP("../data/pixel-based/predictions/randomforest-onestep-walltowall.tif")
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

BMPs = list.files("../data/pixel-based/predictions/", pattern=glob2rx("*.bmp"), full.names=TRUE)
library(gdalUtils)
lapply(BMPs, function(BMP) {gdal_translate(BMP, paste0(strtrim(BMP, nchar(BMP)-4), ".png"), options=c("ZLEVEL=9", "NBITS=1"))})


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
plot(Val.sp["shrub"])
PR.sp = st_set_geometry(PredictionResult, Val.sp[["geometry"]])
plot(PR.sp["shrub"])



plot(PR.ras)
Val.ras = rasterize(Val.sp[Classes], rast, fun=max)
plot(Val.ras)

plotRGB(Val.ras, "shrub", "tree", "grassland", stretch="lin")
plotRGB(PR.ras, "shrub", "tree", "grassland", stretch="lin")
writeRaster(PR.ras, "../rf-2m-raster.tif")
writeRaster(Val.ras, "../validation-raster.tif")

