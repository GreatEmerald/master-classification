# Partial least squares regression
library(pls)
source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("utils/accuracy-statistics.r")
source("pixel-based/utils/crossvalidation.r")
source("pixel-based/utils/subpixel-confusion-matrix.r")

Data.df = LoadTrainingAndCovariates()
class(Data.df) = "data.frame"
Data.df = AddZeroValueColumns(Data.df)
Data.df = TidyData(Data.df)

FullFormula = paste(paste0("cbind(", paste(GetLargeClassNames(Data.df), collapse=","), ")"), paste0(GetAllPixelCovars(), collapse="+"), sep="~")

pm = plsr(FullFormula, data=Data.df)
pm_pred = predict(pm, Data.df, comps=pm$ncomp)
AccuracyStatisticsPlots(pm_pred[,,1], Data.df[,dimnames(pm_pred)[[2]]]) # 24 RMSE, that's almost as bad as intercept
AccuracyStatisticsPlots(pm_pred[,,32], Data.df[,dimnames(pm_pred)[[2]]]) # 18 RMSE, now that's nice

for (i in 1:32) # The more the better
    print(AccuracyStats(pm_pred[,,i], Data.df[,dimnames(pm_pred)[[2]]]))

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
