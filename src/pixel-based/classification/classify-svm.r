# Support vector machine binary relevance
#library(e1071) # too slow
library(liquidSVM)
library(hydroGOF)
library(future)
plan("multiprocess")

source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("pixel-based/utils/crossvalidation.r")
source("pixel-based/utils/subpixel-confusion-matrix.r")
source("utils/accuracy-statistics.r")

Data.df = LoadTrainingAndCovariates()
Data.df = st_set_geometry(Data.df, NULL)
# Manually rescale
Data.df[,GetAllPixelCovars()] = as.matrix(apply(Data.df[,GetAllPixelCovars()], 2, scale))
Data.df[is.na(Data.df)] = -9999
Data.df = TidyData(Data.df, drop.cols=NULL)

# Validation data
Data.val = LoadValidationAndCovariates()
Data.val = st_set_geometry(Data.val, NULL)
Data.val[,GetAllPixelCovars()] = as.matrix(apply(Data.val[,GetAllPixelCovars()], 2, scale))
Data.val[is.na(Data.val)] = -9999
Data.val = TidyData(Data.val, drop.cols=NULL) # Drops around 1050

Classes = GetCommonClassNames()
Truth = Data.val[,Classes]

# Thes are too slow, do not finish training in a day
# SVMDefaultTask = future({
#     BinaryRelevance(paste0("~", paste(GetAllPixelCovars(), collapse = "+")), Data.df, svm, Data.val, filename="../data/pixel-based/predictions/svm-defaults.csv")
# })
# resolved(SVMDefaultTask)
# Predictions = value(SVMDefaultTask)
# AccuracyStatisticsPlots(Predictions[,Classes]/100, Truth[,Classes]/100) # RMSE 19.1, MAE 8.4, similar to RF two-model
# SCM(Predictions[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 71%±2, kappa 0.62±0.03
# NSE(unlist(Predictions[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.59
# PlotHex(Predictions[,Classes], Truth[,Classes], "Cubist, defaults, NA set to -9999")
# PlotBox(Predictions[,Classes], Truth[,Classes], main="Cubist, defaults, NA set to -9999") # Still the same issue with underprediction
# 
# TunedSVM = function(formula, data, ...)
# {
#     SVMTuning = tune(svm, formula, data = data, ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9)))
#     return(SVMTuning$best.model)
# }
# 
# SVMTunedTask = future({
#     BinaryRelevance(paste0("~", paste(GetAllPixelCovars(), collapse = "+")), Data.df, TunedSVM, Data.val, filename="../data/pixel-based/predictions/svm-defaults.csv")
# })
# resolved(SVMTunedTask)
# Predictions = value(SVMTunedTask)
# AccuracyStatisticsPlots(Predictions[,Classes]/100, Truth[,Classes]/100) # RMSE 19.1, MAE 8.4, similar to RF two-model
# SCM(Predictions[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 71%±2, kappa 0.62±0.03
# NSE(unlist(Predictions[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.59
# PlotHex(Predictions[,Classes], Truth[,Classes], "Cubist, defaults, NA set to -9999")
# PlotBox(Predictions[,Classes], Truth[,Classes], main="Cubist, defaults, NA set to -9999") # Still the same issue with underprediction

lsSVMW = function(formula, data, scale_classifier=FALSE, ...) lsSVM(x=formula,y=data, scale=scale_classifier, ...)
qtSVMW = function(formula, data, scale_classifier=FALSE, ...) qtSVM(x=formula,y=data, scale=scale_classifier, ...)

# Unfortunately due to internal threading it doesn't work well with futures
Predictions = BinaryRelevance(paste0("~", paste(GetAllPixelCovars(), collapse = "+")), Data.df, lsSVMW,Data.val, filename="../data/pixel-based/predictions/svm-defaults.csv")
# We get negative values! Set those to zero and rescale.
Predictions[Predictions < 0] = 0
Predictions = ScalePredictions(Predictions, FALSE)
AccuracyStatisticsPlots(Predictions[,Classes]/100, Truth[,Classes]/100) # RMSE 20.4, MAE 11.2, pretty poor but in the ballpark
SCM(Predictions[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 61%±4, kappa 0.49±0.05
NSE(unlist(Predictions[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.53
PlotHex(Predictions[,Classes], Truth[,Classes], "SVM, defaults, NA set to -9999")
PlotBox(Predictions[,Classes], Truth[,Classes], TRUE, main="SVM, defaults, NA set to -9999") # Still the same issue with underprediction

# It seems that >100 also occurs, try manual scaling
Predictions = BinaryRelevance(paste0("~", paste(GetAllPixelCovars(), collapse = "+")), Data.df, lsSVMW, Data.val, scale=FALSE, filename="../data/pixel-based/predictions/svm-scale.csv")
Predictions[Predictions < 0] = 0
Predictions[Predictions > 100] = 100
Predictions = ScalePredictions(Predictions, FALSE)
AccuracyStatisticsPlots(Predictions[,Classes]/100, Truth[,Classes]/100) # RMSE 18.6, MAE 9.9, close to RF single model but not quite there
SCM(Predictions[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 65%±4, kappa 0.55±0.05
NSE(unlist(Predictions[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.61
PlotHex(Predictions[,Classes], Truth[,Classes], "SVM, scaled")
PlotBox(Predictions[,Classes], Truth[,Classes], TRUE, main="SVM, scaled") # Still the same issue with underprediction

# Quantile SVM - medians; suggestion to set max_gamma to 25
Predictions = BinaryRelevance(paste0("~", paste(GetAllPixelCovars(), collapse = "+")), Data.df, qtSVMW, weights=0.5, threads=11, scale=FALSE, Data.val, filename="../data/pixel-based/predictions/svm-median.csv")
# We get negative values! Set those to zero and rescale.
Predictions[Predictions < 0] = 0
Predictions[Predictions > 100] = 100
Predictions = ScalePredictions(Predictions, FALSE)
AccuracyStatisticsPlots(Predictions[,Classes]/100, Truth[,Classes]/100) # RMSE 20.7, MAE 8.9, close to median forest but not quite there
SCM(Predictions[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 69%±2, kappa 0.58±0.03
NSE(unlist(Predictions[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.52
PlotHex(Predictions[,Classes], Truth[,Classes], "SVM, median")
PlotBox(Predictions[,Classes], Truth[,Classes], TRUE, main="SVM, median") # Still the same issue with underprediction

# Try scale and max_gamma
Predictions = BinaryRelevance(paste0("~", paste(GetAllPixelCovars(), collapse = "+")), Data.df, qtSVMW, weights=0.5, threads=11, scale=FALSE, max_gamma=25, Data.val, filename="../data/pixel-based/predictions/svm-median-maxgamma25.csv")
# We get negative values! Set those to zero and rescale.
Predictions[Predictions < 0] = 0
Predictions[Predictions > 100] = 100
Predictions = ScalePredictions(Predictions, FALSE)
AccuracyStatisticsPlots(Predictions[,Classes]/100, Truth[,Classes]/100) # RMSE 20.7, MAE 8.9, no change
SCM(Predictions[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 69%±2, kappa 0.58±0.03
NSE(unlist(Predictions[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.52
PlotHex(Predictions[,Classes], Truth[,Classes], "SVM, median")
PlotBox(Predictions[,Classes], Truth[,Classes], TRUE, main="SVM, median") # Still the same issue with underprediction

