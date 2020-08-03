# SuperLearner binary relevance
library(SuperLearner)

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

SuperLearner.formula = function(formula, data, ...)
{
    MF = model.frame(formula, data=data)
    SuperLearner(MF[[1]], MF[,-1], ...)
}

SLPredict = function(object, newdata, ...)
{
    predict(object, newdata, onlySL = TRUE)$pred
}

# Train the baseline models, excluding SVM because it is too slow
SLRangerTask = future({
    BinaryRelevance(paste0("~", paste(GetAllPixelCovars(), collapse = "+")), Data.df, SuperLearner.formula, Data.val, SLPredict,
        SL.library=c("SL.ranger"), cvControl=list(V=3),
        filename="../data/pixel-based/predictions/superlearner-baseline.csv")
})
resolved(SLRangerTask)
Predictions = value(SLRangerTask)
AccuracyStatisticsPlots(Predictions[,Classes]/100, Truth[,Classes]/100) # RMSE 18.2, MAE 10.6, actually worse than regular ranger!
SCM(Predictions[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 63%±4, kappa 0.52±0.05
NSE(unlist(Predictions[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.63
#PlotHex(Predictions[,Classes], Truth[,Classes], "Cubist, defaults, NA set to -9999")
#PlotBox(Predictions[,Classes], Truth[,Classes], main="Cubist, defaults, NA set to -9999") # Still the same issue with underprediction
