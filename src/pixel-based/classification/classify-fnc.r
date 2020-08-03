library(GSIF) # From devtools::install_github("GreatEmerald/GSIF")
library(hydroGOF)
library(doParallel)
source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("pixel-based/utils/subpixel-confusion-matrix.r") # Replace with package eventually
source("utils/accuracy-statistics.r")
source("pixel-based/utils/crossvalidation.r")
source("utils/fnc-centroids.r")

Data.orig = LoadTrainingAndCovariates()
Data.orig = st_set_geometry(Data.orig, NULL)
Data.df = Data.orig
# Manually rescale
#Data.df = RescaleBasedOn(Data.df, Data.orig, GetAllPixelCovars())
Data.df[is.na(Data.df)] = 0
#Data.df[,GetAllPixelCovars()] = as.matrix(apply(Data.df[,GetAllPixelCovars()], 2, scale))
#Data.df[is.na(Data.df)] = -9999
Data.df = TidyData(Data.df, drop.cols=NULL)

# Validation data
Data.val = LoadValidationAndCovariates()
Data.val = st_set_geometry(Data.val, NULL)
#Data.val = RescaleBasedOn(Data.val, Data.orig, GetAllPixelCovars())
#Data.val[,GetAllPixelCovars()] = as.matrix(apply(Data.val[,GetAllPixelCovars()], 2, scale))
Data.val[is.na(Data.val)] = 0
Data.val = TidyData(Data.val, drop.cols=NULL) # Drops around 1050

Classes = GetCommonClassNames()
Truth = Data.val[,Classes]

FullFormula = paste("dominant_lc", paste0(GetAllPixelCovars(), collapse="+"), sep="~")
FullFormula = formula(FullFormula)

# spfkm does not have a split between training and prediction.
# In fact, the training is the derivation of the class centre and SD;
# all the rest is prediction. So it cn be split out manually.

train_wm.spfkm = function(formula, data, ...)
{
    return(list(class.c=GetClassMeans(formula, data), class.sd=GetClassSDs(formula, data)))
}

# Function to emulate the logistic regression training method
# Not reliable right now: if the logistic regression never predicts that a class will appear, then we miss classes and it's impossible to make a full prediction.
train_lr.spfkm = function(formula, ..., data)
{
    #rout = spmultinom(formulaString=formula, data[formula.tools::lhs.vars(formula)], data[,formula.tools::rhs.vars(formula)], class.stats=TRUE, predict.probs=FALSE)
    rout = spfkm(formula, data[formula.tools::lhs.vars(formula)], data[,formula.tools::rhs.vars(formula)])
    return(list(class.c=rout@class.c, class.sd=rout@class.sd))
}

predict.spfkm = function(model, pred_formula, ..., newdata)
{
    return(spfkm(pred_formula, newdata[formula.tools::lhs.vars(pred_formula)], newdata[,formula.tools::rhs.vars(pred_formula)], class.c=model[["class.c"]], class.sd=model[["class.sd"]], ...)@mu)
}

# What is the difference between centroids derived from logistic regression and weighted average
CompleteRows = apply(as.matrix(Data.df[,GetAllPixelCovars()]), 1, function(x)any(is.na(x)))

LogCentroids = spmultinom(formulaString=FullFormula, Data.df["dominant_lc"], Data.df[,GetAllPixelCovars()], class.stats=TRUE, predict.probs=FALSE)
LogCentroids$class.c
SimpleCentroids = GetClassMeans(FullFormula, Data.df)
CentroidDifferences = LogCentroids$class.c - SimpleCentroids
AccuracyStats(LogCentroids$class.c, SimpleCentroids)

LogCentroids$class.sd
SimpleCentroidSDs = GetClassSDs(FullFormula, Data.df)
SDDifferences = LogCentroids$class.sd - SimpleCentroidSDs
AccuracyStats(LogCentroids$class.sd, SimpleCentroidSDs)

# try log centroids
LCModel = spfkm(FullFormula, Data.val["dominant_lc"], Data.val[,GetAllPixelCovars()],
    class.c=LogCentroids$class.c, class.sd=LogCentroids$class.sd)
AccuracyStatisticsPlots(LCModel@mu[,Classes], Truth[,Classes]/100) # RMSE 28.8, MAE 15.4, better than intercept
SCM(LCModel@mu[,Classes], Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 46%±4, kappa 0.34±0.05
NSE(unlist(LCModel@mu[,Classes]), unlist(Truth[,Classes]/100)) # 0.07, barely better than intercept
PlotHex(as.data.frame(LCModel@mu[,Classes]*100), Truth[,Classes], "Fuzzy nearest centroid, centroids from logistic regression")
PlotBox(as.data.frame(LCModel@mu[,Classes]*100), Truth[,Classes], main="Fuzzy nearest centroid, centroids from logistic regression", binpredicted=TRUE)

# How good is it to use weighted means
WMModel = spfkm(FullFormula, Data.val["dominant_lc"], Data.val[,GetAllPixelCovars()],
    class.c=GetClassMeans(FullFormula, Data.df), class.sd=GetClassSDs(FullFormula, Data.df))

# Scaled to original and also set NA to 0
#write.csv(WMModel@mu[,Classes], "../data/pixel-based/predictions/fnc-na0-scaled.csv", row.names=FALSE)
AccuracyStatisticsPlots(WMModel@mu[,Classes], Truth[,Classes]/100) # RMSE 24.5, MAE 13.5, slightly better
SCM(WMModel@mu[,Classes], Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 53%±4, kappa 0.42±0.06
NSE(unlist(WMModel@mu[,Classes]), unlist(Truth[,Classes]/100)) # 0.33
PlotHex(as.data.frame(WMModel@mu[,Classes]*100), Truth[,Classes], "Fuzzy nearest centroid, weighted means")
PlotBox(as.data.frame(WMModel@mu[,Classes]*100), Truth[,Classes], main="Fuzzy nearest centroid, weighted means", binpredicted=TRUE)
write.csv(WMModel@mu[,Classes], "../data/pixel-based/predictions/fnc-na0.csv", row.names=FALSE)

# What if we replace NAs with means instead
Data.val = LoadValidationAndCovariates()
Data.val = st_set_geometry(Data.val, NULL)
#Data.val[is.na(Data.val)] = -9999
Data.val = NAToMean(Data.val, GetAllPixelCovars())
Data.val = TidyData(Data.val, drop.cols=NULL)

# Log
LCModel = spfkm(FullFormula, Data.val["dominant_lc"], Data.val[,GetAllPixelCovars()],
    class.c=LogCentroids$class.c, class.sd=LogCentroids$class.sd)
AccuracyStatisticsPlots(LCModel@mu[,Classes], Truth[,Classes]/100) # RMSE 27.7, MAE 14.7, improved
SCM(LCModel@mu[,Classes], Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 49%±3, kappa 0.36±0.05
NSE(unlist(LCModel@mu[,Classes]), unlist(Truth[,Classes]/100)) # 0.14, slightly better than intercept
PlotHex(as.data.frame(LCModel@mu[,Classes]*100), Truth[,Classes], "Fuzzy nearest centroid, centroids from logistic regression, NA set to mean")
PlotBox(as.data.frame(LCModel@mu[,Classes]*100), Truth[,Classes], main="Fuzzy nearest centroid, centroids from logistic regression, NA set to mean", binpredicted=TRUE)

# How good is it to use weighted means
WMModel = spfkm(FullFormula, Data.val["dominant_lc"], Data.val[,GetAllPixelCovars()],
    class.c=GetClassMeans(FullFormula, Data.df), class.sd=GetClassSDs(FullFormula, Data.df))

AccuracyStatisticsPlots(WMModel@mu[,Classes], Truth[,Classes]/100) # RMSE 25.1, MAE 14.2, improved
SCM(WMModel@mu[,Classes], Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 51%±5, kappa 0.40±0.06
NSE(unlist(WMModel@mu[,Classes]), unlist(Truth[,Classes]/100)) # 0.30
PlotHex(as.data.frame(WMModel@mu[,Classes]*100), Truth[,Classes], "Fuzzy nearest centroid, weighted means, NA set to mean")
PlotBox(as.data.frame(WMModel@mu[,Classes]*100), Truth[,Classes], main="Fuzzy nearest centroid, weighted means, NA set to mean", binpredicted=TRUE)

# What if fuzziness is increased
FuzzierLogModel = spfkm(FullFormula, Data.df["dominant_lc"], Data.df[,GetAllPixelCovars()], fuzzy.e=1.5)
AccuracyStatisticsPlots(FuzzierLogModel@mu, Data.df[, colnames(FuzzierLogModel@mu)]/100) # RMSE of 21%
SCM(FuzzierLogModel@mu, as.matrix(Data.df[, colnames(FuzzierLogModel@mu)]/100), plot=TRUE, totals=TRUE, scale=TRUE) # 44% accuracy, kappa 0.35: contradictory

FuzzierWeightModel = spfkm(FullFormula, Data.df["dominant_lc"], Data.df[,GetAllPixelCovars()], class.c=GetClassMeans(FullFormula, Data.df), class.sd=GetClassSDs(FullFormula, Data.df), fuzzy.e=1.5)
AccuracyStatisticsPlots(FuzzierWeightModel@mu, Data.df[, colnames(FuzzierWeightModel@mu)]/100) # RMSE of 21%
SCM(FuzzierWeightModel@mu, as.matrix(Data.df[, colnames(FuzzierWeightModel@mu)]/100), plot=TRUE, totals=TRUE, scale=TRUE) # 42% accuracy, kappa 0.33: worse even than the logistic centroids

# What if we do crisp classification
CrispLogModel = spfkm(FullFormula, Data.df["dominant_lc"], Data.df[,GetAllPixelCovars()], fuzzy.e=1.01)
AccuracyStatisticsPlots(CrispLogModel@mu, Data.df[, colnames(CrispLogModel@mu)]/100) # RMSE of 29%
SCM(CrispLogModel@mu, as.matrix(Data.df[, colnames(CrispLogModel@mu)]/100), plot=TRUE, totals=TRUE, scale=TRUE) # 46% accuracy, kappa 0.36: poor as well

CrispWeightModel = spfkm(FullFormula, Data.df["dominant_lc"], Data.df[,GetAllPixelCovars()], class.c=GetClassMeans(FullFormula, Data.df), class.sd=GetClassSDs(FullFormula, Data.df), fuzzy.e=1.01)
AccuracyStatisticsPlots(CrispWeightModel@mu, Data.df[, colnames(CrispWeightModel@mu)]/100) # RMSE of 29%, much worse than the intercept model!
SCM(CrispWeightModel@mu, as.matrix(Data.df[, colnames(CrispWeightModel@mu)]/100), plot=TRUE, totals=TRUE, scale=TRUE) # 46% accuracy, kappa 0.37: almost the same as logistic
