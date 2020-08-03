# Predict pixels using Random Forest

library(ranger)
library(hydroGOF)
library(histmatch) # devtools::install_github("krlmlr/histmatch")
#library(caret)
library(future)
plan("multiprocess")
source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("pixel-based/utils/crossvalidation.r")
source("pixel-based/utils/RFTrain.r")
source("pixel-based/utils/subpixel-confusion-matrix.r")
source("utils/accuracy-statistics.r")

Data.df = LoadTrainingAndCovariates()
Data.df[is.na(Data.df)] = -9999
#Data.sp = Data.df
Data.df = st_set_geometry(Data.df, NULL)
Data.df = AddZeroValueColumns(Data.df)
Data.df = TidyData(Data.df, drop.cols=NULL)
#Data.sp = Data.sp[rownames(Data.df),]

# Make sure there are no NAs left
apply(Data.df[,GetAllPixelCovars()], 2, function(x){sum(is.na(x))}) / nrow(Data.df) * 100

# Validation data
Data.val = LoadValidationAndCovariates()
Data.val[is.na(Data.val)] = -9999
Val.sp = Data.val
Data.val = st_set_geometry(Data.val, NULL)
Data.val = TidyData(Data.val, drop.cols=NULL) # Drops around 1050
Val.sp = Val.sp[rownames(Data.val),]

apply(Data.val, 2, function(x){sum(is.na(x))}) / nrow(Data.df) * 100

Classes = GetCommonClassNames()
Truth = Data.val[,Classes]
# Add column for purity
Data.df$pure = apply(Data.df[,Classes], 1, max) > 95 # At 95%, half of our data is pure
Data.val$pure = apply(Data.val[,Classes], 1, max) > 95 # At 95%, 42% of our validation is pure

# Load ecozone clusters
#EClusters = st_read("../data/pixel-based/biomes/ProbaV_UTM_LC100_biome_clusters_V3_global.gpkg")
# Buffer in QGIS because sf is too slow
#EClusters = st_read("../data/pixel-based/biomes/ProbaV_UTM_LC100_biome_clusters_buffered.gpkg")
#res = st_intersects(EClusters)

#PointTotal = numeric()
#for (cid in 1:nrow(EClusters))
#    PointTotal = c(PointTotal, nrow(st_intersection(Val.sp, EClusters[cid,])))

# We have zero- and 100-inflation in the data.
# If we adjust for the zero inflation and use zero-truncated data for the second model, it tends to just predict 100.
# If we use both zero- and 100-truncated data, it still tends to 100, but is more fuzzy.
## See RFTrain.r for the RFTrain function

## Training per cluster

RFClusterTrain = function(outdir, filename, scale=TRUE, covars=GetAllPixelCovars(), overwrite=FALSE, PredictType="response", PredictQuantiles=0.5, ...)
{
    if (!dir.exists(outdir))
        dir.create(outdir)
    OutputFile = file.path(outdir, paste0("predictions-", filename))
    if (!overwrite && file.exists(OutputFile))
    {
        Predictions = read.csv(OutputFile)
        if (scale) Predictions = ScalePredictions(Predictions)
        return(Predictions)
    }
    quantreg = ifelse(PredictType=="response", FALSE, TRUE)
    
    Covariates = covars
    FullFormula = paste0("~", paste(Covariates, collapse = "+"))
    TrainingSet = Data.df
    ValidationSet = Data.val[!is.na(Data.val$bc_id),]
        
    Predictions = matrix(ncol=length(Classes), nrow=nrow(ValidationSet), dimnames=list(list(), Classes))
    for (Class in Classes)
    {
        print(Class)
        
        RelevantRows = TrainingSet[[Class]] > 0 # Could also just look at dominant
        RemainingCovars = !apply(TrainingSet[RelevantRows, Covariates], 2, function(x){any(!is.finite(x))})
        RemainingNames = names(RemainingCovars)[RemainingCovars]
        
        Formula =  formula(paste0(Class, "~", paste(RemainingNames, collapse = "+")))
        
        # Predict all
        ClassPredictions = ClusterTrain(Formula, TrainingSet, function(...) ranger(..., quantreg=quantreg), ValidationSet, function(x, newdata) predict(x, data=newdata, type=PredictType, quantiles=PredictQuantiles)$prediction, ...)
        Predictions[,Class] = ClassPredictions
    }
    
    write.csv(Predictions, OutputFile, row.names=FALSE)
    
    if (scale) Predictions = ScalePredictions(Predictions)
    
    return(as.data.frame(Predictions))
}

# Actually, oversampling in this case is not needed, because we have a model per class (or two). Y is not unbalanced in that case, just zero-inflated (to various degrees).

# Holdout validation
PredictionResult = RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-allcovars-validation.csv", InflationAdjustment = 0)
#PredictionResult = PredictionUnscaled / rowSums(PredictionUnscaled) * 100
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 17.3%, MAE 9.4%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 67%±4, kappa 0.57±0.05
NSE(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.66

# Holdout with the ~100 uncorrelated covars
PredictionUnscaled = RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-uncorrelated-validation.csv", InflationAdjustment = 0, covars=GetUncorrelatedPixelCovars(), scale=FALSE)
PredictionResult = PredictionUnscaled / rowSums(PredictionUnscaled) * 100
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 16.6%, MAE 9.2%, pretty much no change but much better to explain
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 68%±4, kappa 0.60
NSE(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.67
NSE(PredictionResult[,Classes]/100, Truth[,Classes]/100) # All decent
PlotHex(PredictionResult[,Classes], Truth[,Classes], "RF, single model, uncorrelated covariates")
PlotBox(PredictionResult[,Classes], Truth[,Classes], main="RF, single model, uncorrelated covariates")
# Temp: more plot types to test
ggplot(data.frame(Prediction=unlist(PredictionResult[,Classes]), Truth=unlist(Truth[,Classes])), aes(Truth, Prediction)) +
        stat_density_2d(geom = "point", aes(size = stat(sqrt(density))), n = 50, contour = FALSE) + scale_size_area(max_size=10) +
        geom_abline(slope=1, intercept=0)
ggplot(data.frame(Prediction=unlist(PredictionResult[,Classes]), Truth=unlist(Truth[,Classes])), aes(Truth, Prediction)) +
        stat_density_2d(geom = "raster", aes(fill = stat(sqrt(density))), n=50, contour = FALSE) + scale_fill_distiller(palette="Spectral") +
        geom_abline(slope=1, intercept=0)
ggplot(data.frame(Prediction=unlist(PredictionResult[,Classes]), Truth=unlist(Truth[,Classes])), aes(Truth, Prediction)) +
        geom_point(alpha=0.01, size=2) + 
        geom_abline(slope=1, intercept=0)

OneToOneStatPlot(PredictionResult[,Classes], Truth[,Classes], "RF, single model, uncorrelated covariates")

# Try more variables
PredictionResult <- RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-mtry16.csv", InflationAdjustment = 0, mtry=16)
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 17.3%, MAE 9.4%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 67%±4, kappa 0.58±0.05
NSE(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.67
# Just slightly better


# Histogram matching
HMPredictions = HistMatchPredictions(PredictionUnscaled[,Classes], Data.df[,Classes])
AccuracyStatisticsPlots(HMPredictions/100, Truth[,Classes]/100) # RMSE 22.0%, MAE 9.7%, awful, but ME is collapsed
SCM(HMPredictions/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 66%±2, kappa 0.58±0.03
NSE(unlist(HMPredictions/100), unlist(Truth[,Classes]/100)) # 0.43
NSE(HMPredictions/100, Truth[,Classes]/100) # All decent but quite a bit lower in comparison
PlotHex(HMPredictions, Truth[,Classes], "RF, single model, uncorrelated covariates, histogram matched")
PlotBox(HMPredictions, Truth[,Classes], main="RF, single model, uncorrelated covariates, histogram matched")
OneToOneStatPlot(HMPredictions, Truth[,Classes], "RF, single model, uncorrelated covariates, histogram matched") # 0 and 100 accuracy skyrockets, but the middle plummets

# 2-step
PredictionResult2S <- RFTrain("../data/pixel-based/predictions/", "randomforest-twostep-truncated-validation.csv", InflationAdjustment = 1, TruncateZeroes = TRUE)
PredictionResult2S[rowSums(PredictionResult2S) == 0,] = rep(100/length(Classes),length(Classes)) # Set cases of all 0 to equal
AccuracyStatisticsPlots(PredictionResult2S[,Classes]/100, Truth[,Classes]/100) # RMSE 19.9%, MAE 8.2%
SCM(PredictionResult2S[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 71±2%, kappa 0.62 - this is much better
cor(unlist(PredictionResult2S[,Classes]/100), unlist(Truth[,Classes]/100))^2 # 0.60
NSE(unlist(PredictionResult2S[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.56
NSE(PredictionResult2S[,Classes]/100, Truth[,Classes]/100)
PlotHex(PredictionResult2S[,"shrub"], Truth[,"shrub"], "RF shrubs, two models, zeroes truncated, uncorrelated covariates")
PlotHex(PredictionResult2S[,"tree"], Truth[,"tree"], "RF trees, two models, zeroes truncated, uncorrelated covariates")
PlotHex(PredictionResult2S[,Classes], Truth[,Classes], "RF, two models, zeroes truncated, uncorrelated covariates")
PlotBox(PredictionResult2S[,Classes], Truth[,Classes], main="RF, two models, zeroes truncated, uncorrelated covariates")
OneToOneStatPlot(PredictionResult[,Classes], Truth[,Classes], "RF, two models, zeroes truncated, uncorrelated covariates")
HMPredictions = HistMatchPredictions(PredictionResult[,Classes], Data.df[,Classes])
AccuracyStatisticsPlots(HMPredictions/100, Truth[,Classes]/100) # RMSE 18.8%, MAE 8.0%
SCM(HMPredictions/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 72±2%, kappa 0.63 - no change
NSE(unlist(HMPredictions/100), unlist(Truth[,Classes]/100)) # 0.56
NSE(HMPredictions/100, Truth[,Classes]/100) # Very bad ones got better, but good ones got worse
PlotHex(HMPredictions, Truth[,Classes], "RF, two models, zeroes truncated, uncorrelated covariates, histogram matched")
PlotBox(HMPredictions, Truth[,Classes], main="RF, two models, zeroes truncated, uncorrelated covariates, histogram matched")
OneToOneStatPlot(HMPredictions, Truth[,Classes], "RF, two models, zeroes truncated, uncorrelated covariates, histogram matched") # Worse across the board
svg("../rf-2m-mae.svg", width=9, height=3)
barplot(as.matrix(AccuracyStatTable(PredictionResult[,Classes], Truth[,Classes]))[,"MAE"], main="Mean Absolute Error")
dev.off()
svg("../rf-2m-me.svg", width=9, height=3)
barplot(as.matrix(AccuracyStatTable(PredictionResult[,Classes], Truth[,Classes]))[,"ME"], main="Mean Error (bias)")
dev.off()


# Three-model approach


# Higher purity
PredictionResult = RFTrain3("../data/pixel-based/predictions/", "randomforest-threestep-p99-allcovars-validation.csv", purity_threshold=99)
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 17.3%, MAE 8.3%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 71±4%, kappa 0.64 - worse, closer to one step
cor(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100))^2 # 0.65

# Lower purity
PredictionResult = RFTrain3("../data/pixel-based/predictions/", "randomforest-threestep-p85-allcovars-validation.csv", purity_threshold=85)
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 19.3%, MAE 7.9%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 72±3%, kappa 0.65 - very similar to 95% purity
cor(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100))^2 # 0.65

# Three-model approach with uncorrelated covars
PredictionResult = RFTrain3("../data/pixel-based/predictions/", "randomforest-threestep-validation.csv")
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 19.4%, MAE 8.4% - lower RMSE, higher MAE
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 71±3%, kappa 0.62±0.04 - little difference from two-step model
cor(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100))^2 # 0.63
NSE(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.58 - slightly better than 2S
NSE(PredictionResult[,Classes]/100, Truth[,Classes]/100)
PlotHex(PredictionResult[,Classes], Truth[,Classes], "RF, three models, uncorrelated covariates")
# ggsave("../2019-05-09-rf-3m-uncor-hex.pdf", width=5, height=4)
PlotBox(PredictionResult[,Classes], Truth[,Classes], main="RF, three models, uncorrelated covariates")
OneToOneStatPlot(PredictionResult[,Classes], Truth[,Classes], "RF, three models, uncorrelated covariates")
ResidualBubblePlot(PredictionResult[,Classes], Truth[,Classes], Val.sp[["geometry"]])
HMPredictions = HistMatchPredictions(PredictionUnscaled[,Classes], Data.df[,Classes])
AccuracyStatisticsPlots(HMPredictions/100, Truth[,Classes]/100) # RMSE 21.3%, MAE 9.1%
SCM(HMPredictions/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 68±3%, kappa 0.61
NSE(unlist(HMPredictions/100), unlist(Truth[,Classes]/100)) # 0.46
NSE(HMPredictions/100, Truth[,Classes]/100) # shrubs are awful
PlotHex(HMPredictions, Truth[,Classes], "RF, three models, uncorrelated covariates, histogram matched")
PlotBox(HMPredictions, Truth[,Classes], main="RF, three models, uncorrelated covariates, histogram matched")
OneToOneStatPlot(HMPredictions, Truth[,Classes], "RF, three models, uncorrelated covariates, histogram matched") # Helps zeroes, but really hurts the middle
# Excluding extremes
HMPredictions = HistMatchPredictions(PredictionUnscaled[,Classes], Data.df[,Classes], extremes=0)
AccuracyStatisticsPlots(HMPredictions/100, Truth[,Classes]/100) # RMSE 20.3%, MAE 9.9%
SCM(HMPredictions/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 66±8%, kappa 0.58
NSE(unlist(HMPredictions/100), unlist(Truth[,Classes]/100)) # 0.51
NSE(HMPredictions/100, Truth[,Classes]/100) # Not terrible now
PlotHex(HMPredictions, Truth[,Classes], "RF, three models, uncorrelated covariates, histogram matched except extremes (A)")
PlotBox(HMPredictions, Truth[,Classes], main="RF, three models, uncorrelated covariates, histogram matched except extremes (A)")
OneToOneStatPlot(HMPredictions, Truth[,Classes], "RF, three models, uncorrelated covariates, histogram matched except extremes (A)")
HMPredictions = HistMatchPredictions(PredictionUnscaled[,Classes], Data.df[,Classes], extremes=-1)
AccuracyStatisticsPlots(HMPredictions/100, Truth[,Classes]/100) # RMSE 20.6%, MAE 9.1%
SCM(HMPredictions/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 66±8%, kappa 0.60
NSE(unlist(HMPredictions/100), unlist(Truth[,Classes]/100)) # 0.50
NSE(HMPredictions/100, Truth[,Classes]/100) # Terrible again
PlotHex(HMPredictions, Truth[,Classes], "RF, three models, uncorrelated covariates, histogram matched except extremes (B)")
PlotBox(HMPredictions, Truth[,Classes], main="RF, three models, uncorrelated covariates, histogram matched except extremes (B)")
OneToOneStatPlot(HMPredictions, Truth[,Classes], "RF, three models, uncorrelated covariates, histogram matched except extremes (B)")

# Pure model
Covariates = GetAllPixelCovars()
FullFormula = paste0("~", paste(Covariates, collapse = "+"))
PureFormula = update.formula(FullFormula, paste0("as.factor(pure) ~ ."))
PureModel = ranger(PureFormula, Data.df)
PurityPredictions = predict(PureModel, Data.val)$prediction
PurityPredictions = ifelse(PurityPredictions == "TRUE", TRUE, FALSE)
AccuracyStats(PurityPredictions, Data.val$pure)
mean(PurityPredictions == Data.val$pure) # 81% accuracy, 85% if using 100% as a threshold; you can guess 50% of the times

PureData = Data.val[PurityPredictions,]
ClassFormula = update.formula(FullFormula, paste0("dominant.lc ~ ."))
ClassModel = ranger(ClassFormula, Data.val[PurityPredictions,])

## Test all covars and fewer training vs fewer covars and more training
Data.df = LoadTrainingAndCovariates()
Data.df = as.data.frame(Data.df)
Data.df = AddZeroValueColumns(Data.df)
Data.df = TidyData(Data.df)
Data.val = LoadValidationAndCovariates()
class(Data.val) = "data.frame"
Data.val = TidyData(Data.val) # Drops around 450 # Now drops ~800!
Classes = GetCommonClassNames()
Truth = Data.val[,Classes]
PredictionResult = RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-uncorrelated-validation-nabyrow.csv", InflationAdjustment = 0, covars=GetUncorrelatedPixelCovars())
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 16.4%, MAE 9.1%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 68%±4, kappa 0.59
NSE(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.67
NSE(PredictionResult[,Classes]/100, Truth[,Classes]/100) # All decent
PlotHex(PredictionResult[,Classes], Truth[,Classes], "RF, single model, uncorrelated covariates, NA removed by row")
PlotBox(PredictionResult[,Classes], Truth[,Classes], main="RF, single model, uncorrelated covariates, NA removed by row")
OneToOneStatPlot(PredictionResult[,Classes], Truth[,Classes], "RF, single model, uncorrelated covariates, NA removed by row")




Data.df = LoadTrainingAndCovariates()
Data.df = as.data.frame(Data.df)
Data.df = AddZeroValueColumns(Data.df)
Data.df[is.na(Data.df)] = -1
Data.df = TidyData(Data.df) # 28000 vs 26000
Data.val = LoadValidationAndCovariates()
class(Data.val) = "data.frame"
Data.val[is.na(Data.val)] = -1
Data.val = TidyData(Data.val) # 3500 vs 2700
Classes = GetCommonClassNames()
Truth = Data.val[,Classes]
PredictionResult = RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-uncorrelated-validation-naminusone.csv", InflationAdjustment = 0, covars=GetUncorrelatedPixelCovars())
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 16.6%, MAE 9.2%, slight increase
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 68%±4, kappa 0.60
NSE(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.67
NSE(PredictionResult[,Classes]/100, Truth[,Classes]/100) # All decent, water is much better
PlotHex(PredictionResult[,Classes], Truth[,Classes], "RF, single model, uncorrelated covariates, NA set to -1")
PlotBox(PredictionResult[,Classes], Truth[,Classes], main="RF, single model, uncorrelated covariates, NA set to -1")
OneToOneStatPlot(PredictionResult[,Classes], Truth[,Classes], "RF, single model, uncorrelated covariates, NA set to -1")
# Water is much better, bare soil is much worse

# what if we set -9999?
Data.df = LoadTrainingAndCovariates()
Data.df = as.data.frame(Data.df)
Data.df = AddZeroValueColumns(Data.df)
Data.df[is.na(Data.df)] = -9999
Data.df = TidyData(Data.df) # 28000 vs 26000
Data.val = LoadValidationAndCovariates()
class(Data.val) = "data.frame"
Data.val[is.na(Data.val)] = -9999
Data.val = TidyData(Data.val) # 3500 vs 2700
Classes = GetCommonClassNames()
Truth = Data.val[,Classes]
PredictionResult = RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-uncorrelated-validation-naminusnine.csv", InflationAdjustment = 0, covars=GetUncorrelatedPixelCovars())
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 16.6%, MAE 9.1%, slightly better
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 68%±4, kappa 0.60
NSE(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.67
NSE(PredictionResult[,Classes]/100, Truth[,Classes]/100) # All decent, water is much better
PlotHex(PredictionResult[,Classes], Truth[,Classes], "RF, single model, uncorrelated covariates, NA set to -9999")
PlotBox(PredictionResult[,Classes], Truth[,Classes], main="RF, single model, uncorrelated covariates, NA set to -9999")
OneToOneStatPlot(PredictionResult[,Classes], Truth[,Classes], "RF, single model, uncorrelated covariates, NA set to -9999")
# No change

# And how about if we filter out the bare soil
Data.df = LoadTrainingAndCovariates()
Data.df = as.data.frame(Data.df)
Data.df = AddZeroValueColumns(Data.df)
Data.df = TidyData(Data.df, drop.cols=c("co", "slope")) # 28500 still
Data.df[is.na(Data.df)] = -9999
Data.df = TidyData(Data.df) # 28000
Data.val = LoadValidationAndCovariates()
class(Data.val) = "data.frame"
Data.val = TidyData(Data.val, drop.cols=c("co", "slope")) # 3500
Data.val[is.na(Data.val)] = -9999
Data.val = TidyData(Data.val) # 3500
Classes = GetCommonClassNames()
Truth = Data.val[,Classes]
PredictionResult = RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-uncorrelated-validation-naminusnine-noslope.csv", InflationAdjustment = 0, covars=GetUncorrelatedPixelCovars())
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 16.6%, MAE 9.2%, slightly worse again
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 68%±4, kappa 0.60
NSE(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.68
NSE(PredictionResult[,Classes]/100, Truth[,Classes]/100) # All decent, water is much better
PlotHex(PredictionResult[,Classes], Truth[,Classes], "RF, single model, uncorrelated covariates, NA set to -9999")
PlotBox(PredictionResult[,Classes], Truth[,Classes], main="RF, single model, uncorrelated covariates, NA set to -9999")
OneToOneStatPlot(PredictionResult[,Classes], Truth[,Classes], "RF, single model, uncorrelated covariates, NA set to -9999")
# No change
# So just filling with -9999 is fine

## Try to histmatch only the middle

# Test simply without double rescaling
PredictionResultUnscaled = RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-uncorrelated-unscaled-validation.csv", InflationAdjustment = 0, covars=GetUncorrelatedPixelCovars(), scale=FALSE)
PredictionResult = PredictionResultUnscaled / rowSums(PredictionResultUnscaled) * 100
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 16.6%, MAE 9.2%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 68%±4, kappa 0.60
NSE(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.67
NSE(PredictionResult[,Classes]/100, Truth[,Classes]/100) # All decent
PlotHex(PredictionResult[,Classes], Truth[,Classes], "RF, single model, uncorrelated covariates")
PlotBox(PredictionResult[,Classes], Truth[,Classes], main="RF, single model, uncorrelated covariates")
OneToOneStatPlot(PredictionResult[,Classes], Truth[,Classes], "RF, single model, uncorrelated covariates")
# Histogram matching
HMPredictions = HistMatchPredictions(PredictionResultUnscaled[,Classes], Data.df[,Classes])
AccuracyStatisticsPlots(HMPredictions/100, Truth[,Classes]/100) # RMSE 22.0%, MAE 9.7%, terrible
SCM(HMPredictions/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 66%±2, kappa 0.57, this is much worse
NSE(unlist(HMPredictions/100), unlist(Truth[,Classes]/100)) # 0.43
NSE(HMPredictions/100, Truth[,Classes]/100) # Awful, shrubs are off
PlotHex(HMPredictions, Truth[,Classes], "RF, single model, uncorrelated covariates, histogram matched")
PlotBox(HMPredictions, Truth[,Classes], main="RF, single model, uncorrelated covariates, histogram matched")
OneToOneStatPlot(HMPredictions, Truth[,Classes], "RF, single model, uncorrelated covariates, histogram matched") # 0 and 100 accuracy skyrockets, but the middle plummets

# And now if we try to match only the non-extremes
# Type 0: just ignore extreme values
HMPredictions = HistMatchPredictions(PredictionResultUnscaled[,Classes], Data.df[,Classes], extremes=0)
AccuracyStatisticsPlots(HMPredictions/100, Truth[,Classes]/100) # RMSE 22.0%, MAE 14.2%, the worst
SCM(HMPredictions/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 51%±7, kappa 0.42
NSE(unlist(HMPredictions/100), unlist(Truth[,Classes]/100)) # 0.43
NSE(HMPredictions/100, Truth[,Classes]/100) # Negatives
PlotHex(HMPredictions, Truth[,Classes], "RF, single model, uncorrelated covariates, histogram matched except extremes")
PlotBox(HMPredictions, Truth[,Classes], main="RF, single model, uncorrelated covariates, histogram matched except extremes")
OneToOneStatPlot(HMPredictions, Truth[,Classes], "RF, single model, uncorrelated covariates, histogram matched except extremes") # 0 and 100 accuracy skyrockets, but the middle plummets

# That was awful as expected, now to the quantile method (results should about equal the standard method)
HMPredictions = HistMatchPredictions(PredictionResultUnscaled[,Classes], Data.df[,Classes], extremes=-1)
AccuracyStatisticsPlots(HMPredictions/100, Truth[,Classes]/100) # RMSE 18.6%, MAE 9.9%, just a bit worse
SCM(HMPredictions/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 65%±5, kappa 0.57
NSE(unlist(HMPredictions/100), unlist(Truth[,Classes]/100)) # 0.59
NSE(HMPredictions/100, Truth[,Classes]/100) # All positive but all worse
PlotHex(HMPredictions, Truth[,Classes], "RF, single model, uncorrelated covariates, histogram matched except extremes, quantiles")
PlotBox(HMPredictions, Truth[,Classes], main="RF, single model, uncorrelated covariates, histogram matched except extremes, quantiles")
OneToOneStatPlot(HMPredictions, Truth[,Classes], "RF, single model, uncorrelated covariates, histogram matched except extremes, quantiles") # 0 and 100 accuracy skyrockets, but the middle plummets

## Training per cluster

PredictionResult = RFClusterTrain("../data/pixel-based/predictions/", "randomforest-onestep-onecluster-validation.csv")
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 17.7%, MAE 9.5%, very slightly worse
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 67%±3, kappa 0.58±0.05
NSE(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.65

PredictionResult = RFClusterTrain("../data/pixel-based/predictions/", "randomforest-onestep-neighbourcluster-validation.csv", include_neighbours=TRUE)
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 17.5%, MAE 9.3%, almost the same
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 67%±3, kappa 0.58±0.05
NSE(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.66

# Training with cluster as covar
PredictionResult = RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-clustercovar-validation.csv", InflationAdjustment = 0, covars=c(GetAllPixelCovars(), "bc_id"))
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 17.4%, MAE 9.5%, slightly worse
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 67%±4, kappa 0.58±0.05, also slightly worse
NSE(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.66

## Median forest

PredictionResult = RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-median-validation.csv", InflationAdjustment = 0, PredictType="quantiles")
PredictionResult[rowSums(PredictionResult) == 0,] = rep(100/length(Classes),length(Classes)) # Set cases of all 0 to equal
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 20.7%, MAE 7.9%, much better MAE and much worse RMSE, even stronger than 2/3-step
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 72%±1, kappa 0.63±0.01, much better
NSE(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.52, much worse
PlotHex(PredictionResult[,Classes], Truth[,Classes], "RF, single model, median")
PlotBox(PredictionResult[,Classes], Truth[,Classes], main="RF, single model, median")

# Two-step median
PredictionResult <- RFTrain("../data/pixel-based/predictions/", "randomforest-twostep-truncated-median.csv", InflationAdjustment = 1, TruncateZeroes = TRUE, PredictType="quantiles")
PredictionResult[rowSums(PredictionResult) == 0,] = rep(100/length(Classes),length(Classes)) # Set cases of all 0 to equal
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 20.0%, MAE 8.1%, slightly better
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 72±1%, kappa 0.63±0.02, the same as mean
NSE(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.54
PlotHex(PredictionResult[,Classes], Truth[,Classes], "RF, two-step model, median")
PlotBox(PredictionResult[,Classes], Truth[,Classes], main="RF, two-step model, median")

# Three-step median
PredictionResult = RFTrain3("../data/pixel-based/predictions/", "randomforest-threestep-median.csv", PredictType="quantiles")
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 20.2%, MAE 7.9% - overall better than 1-step
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 72±2%, kappa 0.64±0.02 - also better than mean forest
NSE(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.54
NSE(PredictionResult[,Classes]/100, Truth[,Classes]/100)
PlotHex(PredictionResult[,Classes], Truth[,Classes], "RF, three-step model, median")
PlotBox(PredictionResult[,Classes], Truth[,Classes], main="RF, three-step model, median")

## Only RS params
RSCovars = GetAllPixelCovars(TRUE)
RSCovars = c(RSCovars$location, RSCovars$spectral, RSCovars$harmonic)
PredictionResult = RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-rscovars-validation.csv", InflationAdjustment = 0, covars=RSCovars)
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 17.9%, MAE 9.9%, pretty good in general, everything is just around 2% worse
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 65%±4, kappa 0.56±0.05, also slightly worse
NSE(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.64

## Display a comparison between all models

InterceptModel = Truth[,Classes]
InterceptModel[] = 1/length(Classes)

AccuracyStatisticsPlots(InterceptModel, Truth[,Classes]/100) # RMSE 29.9%, MAE 21.4%
SCM(InterceptModel, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 26±5%, kappa 0.13±0.07
NSE(unlist(InterceptModel), unlist(Truth[,Classes]/100)) # 0
PlotBox(InterceptModel*100, Truth[,Classes], main="Intercept model") # It is a line

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
