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

pm = plsr(FullFormula, data=Data.df, scale=TRUE)
pm_pred = predict(pm, Data.val)
pm_pred_n = pm_pred[,,55] # Somehow using predict(comps=) gives the wrong values, but this works
# If we increase the number of components, we get closer to just a single linear regression.
pm_pred_n[pm_pred_n < 0] = 0
pm_pred_n = ScalePredictions(pm_pred_n, FALSE)
AccuracyStatisticsPlots(pm_pred_n[,Classes], Data.val[,Classes]) # RMSE 21.8, MAE 12.9
SCM(pm_pred_n[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 55±0.04 kappa 0.42±0.06
NSE(unlist(as.data.frame(pm_pred_n[,Classes]))/100, unlist(Truth[,Classes]/100)) # 0.47
PlotHex(as.data.frame(pm_pred_n[,Classes]), Truth[,Classes], "PLS regression, 55 components")
PlotBox(as.data.frame(pm_pred_n[,Classes]), Truth[,Classes], main="PLS regression, 55 components", binpredicted=TRUE)
write.csv(pm_pred_n[,Classes], "../data/pixel-based/predictions/plsr-55c.csv", row.names=FALSE)

#AccuracyStatisticsPlots(pm_pred[,,32], Data.df[,dimnames(pm_pred)[[2]]]) # 18 RMSE, now that used to be nice, but can't reproduce on the current version

for (i in 1:pm$ncomp) # Max is best and equal to lm
{
    pm_pred_n = pm_pred[,Classes,i]
    pm_pred_n[pm_pred_n < 0] = 0
    pm_pred_n = ScalePredictions(pm_pred_n, FALSE)
    print(AccuracyStats(pm_pred_n, Data.val[,Classes]))
}
pm = plsr(FullFormula, data=Data.df, center=FALSE) # This is even more terrible
    
#pm_scaled = ScaleNNPrediction(pm_pred[,,32])
#SCM(pm_scaled, Data.df[,dimnames(pm_pred)[[2]]]/100, plot=TRUE, totals=TRUE, scale=TRUE) # Kappa 0.36±0.07, OA 47±5%, pretty bad
#AccuracyStatisticsPlots(pm_scaled, Data.df[,dimnames(pm_pred)[[2]]]/100) # 19 RMSE when scaled

#head(round(pm_scaled, 2)) # Way too fuzzy

# Can CV figure out that max is the best?
pmcv = plsr(FullFormula, data=Data.df, scale=TRUE, validation="CV")
#selectNcomp(pmcv, method="randomization") # Doesn't work with multiple response
plot(pmcv, plottype="validation") # 55 seems about optimal
plot(pmcv, newdata=Data.val, plottype="validation") # What is optimal in reality: max (67) or 62
plot(pmcv, newdata=Data.val) # 1:1 plots

# Other methods
# pm = plsr(FullFormula, data=Data.df, scale=TRUE, method="widekernelpls") # Indeed eats all memory and dies
pm = plsr(FullFormula, data=Data.df, scale=TRUE, method="simpls")
plot(pm, plottype="validation") # Still max is the best
pm = plsr(FullFormula, data=Data.df, scale=TRUE, method="oscorespls")
plot(pm, plottype="validation") # Still max is the best

# # With CV
# # Wrapper for predict to give matrices instead of arrays
# pred.plsr = function(model, newdata, ...)
# {
#     result = predict(model, newdata=newdata, ncomp=model$ncomp, ...)
#     return(result[,,1])
# }
# 
# registerDoParallel(cores=10)
# pm_cv = CrossValidate(FullFormula, Data.df, plsr, pred.plsr)
# pm_cv_scaled = ScaleNNPrediction(pm_cv)
# SCM(pm_cv_scaled, Data.df[,colnames(pm_cv_scaled)]/100, plot=TRUE, totals=TRUE, scale=TRUE) # Kappa 0.36±0.07, OA 47±5%, so it's the same as without CV
# AccuracyStatisticsPlots(pm_cv_scaled, Data.df[,colnames(pm_cv_scaled)]/100) # 19 RMSE
# 
# # What about other methods? widekernelpls does not work (eats all memory and dies)
# pm_cv = CrossValidate(FullFormula, Data.df, plsr, pred.plsr, method="simpls")
# pm_cv_scaled = ScaleNNPrediction(pm_cv)
# SCM(pm_cv_scaled, Data.df[,colnames(pm_cv_scaled)]/100, plot=TRUE, totals=TRUE, scale=TRUE) # Kappa 0.35±0.07, OA 47±5%, just slightly worse
# AccuracyStatisticsPlots(pm_cv_scaled, Data.df[,colnames(pm_cv_scaled)]/100) # 19 RMSE
# 
# pm_cv = CrossValidate(FullFormula, Data.df, plsr, pred.plsr, method="oscorespls")
# pm_cv_scaled = ScaleNNPrediction(pm_cv)
# SCM(pm_cv_scaled, Data.df[,colnames(pm_cv_scaled)]/100, plot=TRUE, totals=TRUE, scale=TRUE) # Kappa 0.36±0.07, OA 47±5%, same as default
# AccuracyStatisticsPlots(pm_cv_scaled, Data.df[,colnames(pm_cv_scaled)]/100) # 19 RMSE
# 
# # PCR
# pm_cv = CrossValidate(FullFormula, Data.df, pcr, pred.plsr)
# pm_cv_scaled = ScaleNNPrediction(pm_cv)
# SCM(pm_cv_scaled, Data.df[,colnames(pm_cv_scaled)]/100, plot=TRUE, totals=TRUE, scale=TRUE) # Kappa 0.36±0.07, OA 47±5%, same as default
# AccuracyStatisticsPlots(pm_cv_scaled, Data.df[,colnames(pm_cv_scaled)]/100) # 19 RMSE
# 
# # What about uncorrelated covars
# FullFormula = paste(paste0("cbind(", paste(GetLargeClassNames(Data.df), collapse=","), ")"), paste0(GetUncorrelatedPixelCovars(), collapse="+"), sep="~")
# pm_cv = CrossValidate(FullFormula, Data.df, plsr, pred.plsr)
# pm_cv_scaled = ScaleNNPrediction(pm_cv)
# SCM(pm_cv_scaled, Data.df[,colnames(pm_cv_scaled)]/100, plot=TRUE, totals=TRUE, scale=TRUE) # Kappa 0.31±0.07, OA 43±5%, so it's worse
# AccuracyStatisticsPlots(pm_cv_scaled, Data.df[,colnames(pm_cv_scaled)]/100) # 20 RMSE
# # This makes a lot of sense: PLSR is designed to cope with collinear variables
