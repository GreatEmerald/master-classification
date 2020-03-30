# Classification using a cubist model
library(Cubist)
library(hydroGOF)

source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("pixel-based/utils/crossvalidation.r")
source("pixel-based/utils/subpixel-confusion-matrix.r")
source("utils/accuracy-statistics.r")

Data.df = LoadTrainingAndCovariates()
Data.df[is.na(Data.df)] = -9999
Data.df = st_set_geometry(Data.df, NULL)
Data.df = TidyData(Data.df, drop.cols=NULL)

# Validation data
Data.val = LoadValidationAndCovariates()
Data.val[is.na(Data.val)] = -9999
Data.val = st_set_geometry(Data.val, NULL)
Data.val = TidyData(Data.val, drop.cols=NULL) # Drops around 1050

Classes = GetCommonClassNames()
Truth = Data.val[,Classes]

# Formula interface, supports only a single formula
cubist.formula = function(formula, data, ...)
{
    MF = model.frame(formula, data=data)
    cubist(MF[,-1], MF[,1], ...)
}

Predictions = BinaryRelevance(paste0("~", paste(GetAllPixelCovars(), collapse = "+")), Data.df, cubist.formula, Data.val, filename="../data/pixel-based/predictions/cubist-defaults.csv")
AccuracyStatisticsPlots(Predictions[,Classes]/100, Truth[,Classes]/100) # RMSE 19.1, MAE 8.4, similar to RF two-model
SCM(Predictions[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 71%±2, kappa 0.62±0.03
NSE(unlist(Predictions[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.59
PlotHex(Predictions[,Classes], Truth[,Classes], "Cubist, defaults, NA set to -9999")
PlotBox(Predictions[,Classes], Truth[,Classes], main="Cubist, defaults, NA set to -9999") # Still the same issue with underprediction

Predictions = BinaryRelevance(paste0("~", paste(GetAllPixelCovars(), collapse = "+")), Data.df, cubist.formula, Data.val, filename="../data/pixel-based/predictions/cubist-defaults-naomit.csv")
AccuracyStatisticsPlots(Predictions[,Classes]/100, Truth[,Classes]/100) # RMSE 20.4, MAE 9.1, worse than -9999
SCM(Predictions[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 68%±2, kappa 0.58±0.03
NSE(unlist(Predictions[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.53
PlotHex(Predictions[,Classes], Truth[,Classes], "Cubist, defaults, NAs omitted")
PlotBox(Predictions[,Classes], Truth[,Classes], main="Cubist, defaults, NAs omitted", binpredicted=TRUE) # Still the same issue with underprediction

PredictionTask = future({BinaryRelevance(paste0("~", paste(GetAllPixelCovars(), collapse = "+")), Data.df, cubist.formula, Data.val, committees=10, filename="../data/pixel-based/predictions/cubist-committees10.csv")})
resolved(PredictionTask)
Predictions = value(PredictionTask)
AccuracyStatisticsPlots(Predictions[,Classes]/100, Truth[,Classes]/100) # RMSE 18.1, MAE 8.1, outright better than RF two-model and three-model
SCM(Predictions[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 72%±2, kappa 0.63±0.03
NSE(unlist(Predictions[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.63
PlotHex(Predictions[,Classes], Truth[,Classes], "Cubist, 10 committees")
PlotBox(Predictions[,Classes], Truth[,Classes], TRUE, main="Cubist, 10 committees")

# Prediction using neighbours
PredictionTaskNeigh5 = future({BinaryRelevance(paste0("~", paste(GetAllPixelCovars(), collapse = "+")), Data.df, cubist.formula, Data.val, neighbors=5, filename="../data/pixel-based/predictions/cubist-neighbours5.csv")})
resolved(PredictionTaskNeigh5)
Predictions = value(PredictionTaskNeigh5)
Predictions = BinaryRelevance(paste0("~", paste(GetAllPixelCovars(), collapse = "+")), Data.df, cubist.formula, Data.val, neighbors=5, filename="../data/pixel-based/predictions/cubist-neighbours5.csv")
AccuracyStatisticsPlots(Predictions[,Classes]/100, Truth[,Classes]/100) # RMSE 18.2, MAE 8.9, lower RMSE but higher MAE
SCM(Predictions[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 69%±3, kappa 0.60±0.04, overall worse than default
NSE(unlist(Predictions[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.63, almost like RF 1-model
PlotHex(Predictions[,Classes], Truth[,Classes], "Cubist, 5 neighbours")
PlotBox(Predictions[,Classes], Truth[,Classes], TRUE, main="Cubist, 5 neighbours")

# Using both neighbours and committees
PredictionTaskNeighCom10 = future({BinaryRelevance(paste0("~", paste(GetAllPixelCovars(), collapse = "+")), Data.df, cubist.formula, Data.val, neighbors=5, committees=10, filename="../data/pixel-based/predictions/cubist-neighbours5-committees10.csv")})
resolved(PredictionTaskNeighCom10)
Predictions = value(PredictionTaskNeighCom10)
AccuracyStatisticsPlots(Predictions[,Classes]/100, Truth[,Classes]/100) # RMSE 17.8, MAE 8.8, similar to RF single-model
SCM(Predictions[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 69%±3, kappa 0.60±0.04
NSE(unlist(Predictions[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.64
PlotHex(Predictions[,Classes], Truth[,Classes], "Cubist, 5 neighbours and 10 commitees")
PlotBox(Predictions[,Classes], Truth[,Classes], TRUE, main="Cubist, 5 neighbours and 10 commitees")
