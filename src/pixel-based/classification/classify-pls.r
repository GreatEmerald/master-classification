# Partial least squares regression
library(pls)
source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("utils/accuracy-statistics.r")
source("pixel-based/utils/crossvalidation.r")
source("pixel-based/utils/subpixel-confusion-matrix.r")

Data.df.orig = LoadTrainingAndCovariates()
Data.df.orig = st_set_geometry(Data.df.orig, NULL)
# Manually rescale
Data.df = Data.df.orig
#Data.df = RescaleBasedOn(Data.df.orig, Data.df.orig, GetAllPixelCovars())
#Data.df = NAToMean(Data.df, GetAllPixelCovars())
Data.df[is.na(Data.df)] = -9999
#Data.df[is.na(Data.df)] = 0
Data.df = TidyData(Data.df, drop.cols=NULL)

# Validation data
Data.val = LoadValidationAndCovariates()
Data.val = st_set_geometry(Data.val, NULL)
#Data.val = RescaleBasedOn(Data.val, Data.df.orig, GetAllPixelCovars())
#Data.val = NAToMean(Data.val, GetAllPixelCovars())
Data.val[is.na(Data.val)] = -9999
#Data.val[is.na(Data.val)] = 0
Data.val = TidyData(Data.val, drop.cols=NULL) # Drops around 1050

AllCovars = GetAllPixelCovars()
Classes = GetCommonClassNames()
Truth = Data.val[,Classes]

FullFormula = paste(paste0("cbind(", paste(Classes, collapse=","), ")"), paste0(GetAllPixelCovars(), collapse="+"), sep="~")

pm = plsr(FullFormula, data=Data.df)
pm_pred = predict(pm, Data.val, comps=2)
pm_pred[pm_pred < 0] = 0
pm_pred = ScalePredictions(pm_pred, FALSE)
AccuracyStatisticsPlots(pm_pred[,Classes], Data.val[,Classes]) # RMSE 27.2, MAE 17.8, not useful
SCM(pm_pred[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 38±0.04 kappa 0.21±0.06
NSE(unlist(as.data.frame(pm_pred[,Classes]))/100, unlist(Truth[,Classes]/100)) # 0.17
PlotHex(as.data.frame(pm_pred[,Classes]), Truth[,Classes], "PLS regression, 2 components")
PlotBox(as.data.frame(pm_pred[,Classes]), Truth[,Classes], main="PLS regression, 2 components", binpredicted=TRUE)

AccuracyStatisticsPlots(pm_pred[,,32], Data.df[,dimnames(pm_pred)[[2]]]) # 18 RMSE, now that used to be nice, but can't reproduce on the current version

for (i in 1:pm$ncomp) # All very bad, 1 is best
{
    pm_pred = predict(pm, Data.val, comps=i)[,Classes]
    pm_pred[pm_pred < 0] = 0
    pm_pred = ScalePredictions(pm_pred, FALSE)
    print(AccuracyStats(pm_pred, Data.val[,Classes]))
}
pm = plsr(FullFormula, data=Data.df, center=FALSE) # This is even more terrible
    
pm_scaled = ScaleNNPrediction(pm_pred[,,32])
SCM(pm_scaled, Data.df[,dimnames(pm_pred)[[2]]]/100, plot=TRUE, totals=TRUE, scale=TRUE) # Kappa 0.36±0.07, OA 47±5%, pretty bad
AccuracyStatisticsPlots(pm_scaled, Data.df[,dimnames(pm_pred)[[2]]]/100) # 19 RMSE when scaled

head(round(pm_scaled, 2)) # Way too fuzzy

# With CV
# Wrapper for predict to give matrices instead of arrays
pred.plsr = function(model, newdata, ...)
{
    result = predict(model, newdata=newdata, ncomp=model$ncomp, ...)
    return(result[,,1])
}

registerDoParallel(cores=10)
pm_cv = CrossValidate(FullFormula, Data.df, plsr, pred.plsr)
pm_cv_scaled = ScaleNNPrediction(pm_cv)
SCM(pm_cv_scaled, Data.df[,colnames(pm_cv_scaled)]/100, plot=TRUE, totals=TRUE, scale=TRUE) # Kappa 0.36±0.07, OA 47±5%, so it's the same as without CV
AccuracyStatisticsPlots(pm_cv_scaled, Data.df[,colnames(pm_cv_scaled)]/100) # 19 RMSE

# What about other methods? widekernelpls does not work (eats all memory and dies)
pm_cv = CrossValidate(FullFormula, Data.df, plsr, pred.plsr, method="simpls")
pm_cv_scaled = ScaleNNPrediction(pm_cv)
SCM(pm_cv_scaled, Data.df[,colnames(pm_cv_scaled)]/100, plot=TRUE, totals=TRUE, scale=TRUE) # Kappa 0.35±0.07, OA 47±5%, just slightly worse
AccuracyStatisticsPlots(pm_cv_scaled, Data.df[,colnames(pm_cv_scaled)]/100) # 19 RMSE

pm_cv = CrossValidate(FullFormula, Data.df, plsr, pred.plsr, method="oscorespls")
pm_cv_scaled = ScaleNNPrediction(pm_cv)
SCM(pm_cv_scaled, Data.df[,colnames(pm_cv_scaled)]/100, plot=TRUE, totals=TRUE, scale=TRUE) # Kappa 0.36±0.07, OA 47±5%, same as default
AccuracyStatisticsPlots(pm_cv_scaled, Data.df[,colnames(pm_cv_scaled)]/100) # 19 RMSE

# PCR
pm_cv = CrossValidate(FullFormula, Data.df, pcr, pred.plsr)
pm_cv_scaled = ScaleNNPrediction(pm_cv)
SCM(pm_cv_scaled, Data.df[,colnames(pm_cv_scaled)]/100, plot=TRUE, totals=TRUE, scale=TRUE) # Kappa 0.36±0.07, OA 47±5%, same as default
AccuracyStatisticsPlots(pm_cv_scaled, Data.df[,colnames(pm_cv_scaled)]/100) # 19 RMSE

# What about uncorrelated covars
FullFormula = paste(paste0("cbind(", paste(GetLargeClassNames(Data.df), collapse=","), ")"), paste0(GetUncorrelatedPixelCovars(), collapse="+"), sep="~")
pm_cv = CrossValidate(FullFormula, Data.df, plsr, pred.plsr)
pm_cv_scaled = ScaleNNPrediction(pm_cv)
SCM(pm_cv_scaled, Data.df[,colnames(pm_cv_scaled)]/100, plot=TRUE, totals=TRUE, scale=TRUE) # Kappa 0.31±0.07, OA 43±5%, so it's worse
AccuracyStatisticsPlots(pm_cv_scaled, Data.df[,colnames(pm_cv_scaled)]/100) # 20 RMSE
# This makes a lot of sense: PLSR is designed to cope with collinear variables
