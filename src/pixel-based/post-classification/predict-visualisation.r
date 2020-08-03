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

## Model comparison plots

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
Cubist = BinaryRelevance(NULL, NULL, NULL, NULL, filename="../data/pixel-based/predictions/cubist-committees10.csv")/100
SVM = BinaryRelevance(NULL, NULL, NULL, NULL, filename="../data/pixel-based/predictions/svm-median.csv")
SVM[SVM < 0] = 0
SVM = ScalePredictions(SVM, FALSE)
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

# Reduced to best per group

ggplotBox(list(Intercept = InterceptModel, GLM=LM, RF = RFSingle, SVM=SVM),
          Truth[,Classes], main="Model comparison", outlier.shape=NA)

# Barplots
RMSEPlot = ggplotBar(list(Intercept = InterceptModel, GLM=LM, SVM=SVM, Cubist=Cubist*100, `RF 1-step mean` = RFSingle, `RF 3-step median` = RFThreeStepMedian)) + theme(legend.position="none", axis.title.x = element_blank())
MAEPlot = ggplotBar(list(Intercept = InterceptModel, GLM=LM, SVM=SVM, Cubist=Cubist*100, `RF 1-step mean` = RFSingle, `RF 3-step median` = RFThreeStepMedian), "MAE") + theme(legend.position="bottom")
pdf("../output/2020-06-03-model-comparison-bar.pdf", width=1272/175, height=634/175)
gridExtra::grid.arrange(RMSEPlot, MAEPlot, heights=c(40, 60))
dev.off()


# Drill-down into Random Forest
RFTwoStep = ScalePredictions(RFTrain("../data/pixel-based/predictions/", "randomforest-twostep-truncated-validation.csv", InflationAdjustment = 1, TruncateZeroes = TRUE))
RFThreeStep = RFTrain3("../data/pixel-based/predictions/", "randomforest-threestep-validation.csv")
RFSingleMedian = ScalePredictions(RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-median-validation.csv", InflationAdjustment = 0, PredictType="quantiles"))
RFTwoStepMedian = ScalePredictions(RFTrain("../data/pixel-based/predictions/", "randomforest-twostep-truncated-median.csv", InflationAdjustment = 1, TruncateZeroes = TRUE, PredictType="quantiles"))
RSLocationCovars = unlist(GetAllPixelCovars(TRUE)[c("spectral", "harmonic", "location")])
RFOnlyRSLocation = RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-rscovars-validation.csv", InflationAdjustment = 0, covars=RSLocationCovars)
RFOnlyRSStrict = RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-rscovars-nolocation-validation.csv", InflationAdjustment = 0, covars=unlist(GetAllPixelCovars(TRUE)[c("spectral", "harmonic")]))

ggplotBox(list(
    Intercept = InterceptModel,
    RFOnlyRSStrict = RFOnlyRSStrict,
    RFOnlyRSLocation = RFOnlyRSLocation,
    RFSingle = RFSingle,
    RFTwoStep = RFTwoStep,
    RFThreeStep = RFThreeStep,
    RFSingleMedian = RFSingleMedian,
    RFTwoStepMedian = RFTwoStepMedian,
    RFThreeStepMedian = RFThreeStepMedian
), Truth[,Classes], main="Random forest model comparison", outlier.shape=NA)

RMSEPlot = ggplotBar(list(
    Intercept = InterceptModel,
    `1-step mean` = RFSingle,
    `2-step mean` = RFTwoStep,
    `3-step mean` = RFThreeStep,
    `1-step median` = RFSingleMedian,
    `3-step median` = RFThreeStepMedian
    )) + theme(legend.position="none", axis.title.x = element_blank())
MAEPlot = ggplotBar(list(
    Intercept = InterceptModel,
    `1-step mean` = RFSingle,
    `2-step mean` = RFTwoStep,
    `3-step mean` = RFThreeStep,
    `1-step median` = RFSingleMedian,
    `3-step median` = RFThreeStepMedian
    ), "MAE") + theme(legend.position="bottom")
pdf("../output/2020-06-04-rf-comparison-bar.pdf", width=1272/175, height=634/175)
gridExtra::grid.arrange(RMSEPlot, MAEPlot, heights=c(40, 60))
dev.off()

AccuracyStatisticsPlots(RFOnlyRSStrict[,Classes], Truth[,Classes]) # 18.4 RMSE, 10.3 MAE
hydroGOF::NSE(unlist(RFOnlyRSStrict[,Classes]), unlist(Truth[,Classes])) # 0.62
SCM(RFOnlyRSStrict[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 0.64±0.04, kappa 0.54±0.05

## Plot 1:1 plots
png("../output/2020-06-21-rf1st-1to1-box-overall.png", width=70.2414*15, height=26.7229*15, res=300)
PlotBox(RFSingle, Truth, binpredicted = TRUE, transposeaxes = TRUE)
dev.off()
for (Class in Classes)
{
    png(paste0("../output/2020-06-21-rf1st-1to1-box-", Class, ".png"), width=70.2414*15, height=26.7229*15, res=300)
    print(PlotBox(RFSingle[,Class], Truth[,Class], binpredicted = TRUE, transposeaxes = TRUE))
    dev.off()
}

# Comparison with Montesano 2009
hydroGOF::NSE(RFSingle, Truth) # NSE of trees at 0.77
cor(RFSingle[,"tree"], Truth[,"tree"])^2 # 0.77 as well
summary(lm(RFSingle[,"tree"]~Truth[,"tree"])) # Significant coeffs, +8.8 intercept and 0.7 slope
summary(lm(Truth[,"tree"]~RFSingle[,"tree"])) # Same this way, -2.5 and 1.09
RMSE(RFSingle[,"tree"], Truth[,"tree"]) # 19.3

# Comparison with Li 2018
MyClass="water"
PlotHex(Cubist[,MyClass]*100, Truth[, MyClass], "Cubist")
hydroGOF::NSE(Cubist[,MyClass]*100, Truth[, MyClass]) # 0.723
cor(Cubist[,MyClass]*100, Truth[, MyClass])^2         # 0.726
AccuracyStats(Cubist[,MyClass]*100, Truth[, MyClass]) # 11.51 RMSE, 2.57 MAE
PlotHex(RFSingle[,MyClass], Truth[, MyClass], "RF")
hydroGOF::NSE(RFSingle[,MyClass], Truth[, MyClass])   # 0.716
cor(RFSingle[,MyClass], Truth[, MyClass])^2           # 0.728
AccuracyStats(RFSingle[,MyClass], Truth[, MyClass])   # 11.65 RMSE, 3.48 MAE

# Comparison with Walton 2008
MyClass="urban_built_up"
PlotHex(Cubist[,MyClass]*100, Truth[, MyClass], "Cubist")
hydroGOF::NSE(Cubist[,MyClass]*100, Truth[, MyClass]) # 0.398
cor(Cubist[,MyClass]*100, Truth[, MyClass])^2         # 0.452
AccuracyStats(Cubist[,MyClass]*100, Truth[, MyClass]) # 9.70 RMSE, 2.43 MAE
PlotHex(RFSingle[,MyClass], Truth[, MyClass], "RF")
hydroGOF::NSE(RFSingle[,MyClass], Truth[, MyClass])   # 0.385
cor(RFSingle[,MyClass], Truth[, MyClass])^2           # 0.510
AccuracyStats(RFSingle[,MyClass], Truth[, MyClass])   # 9.80 RMSE, 3.01 MAE
PlotHex(SVM[,MyClass], Truth[, MyClass], "SVM")
hydroGOF::NSE(SVM[,MyClass], Truth[, MyClass])        # 0.087
cor(SVM[,MyClass], Truth[, MyClass])^2                # 0.134
AccuracyStats(SVM[,MyClass], Truth[, MyClass])        # 11.94 RMSE, 2.86 MAE
