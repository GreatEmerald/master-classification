library(GSIF)
library(doParallel)
source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("pixel-based/utils/subpixel-confusion-matrix.r") # Replace with package eventually
source("utils/accuracy-statistics.r")
source("pixel-based/utils/crossvalidation.r")
source("utils/fnc-centroids.r")

Data.df = LoadTrainingAndCovariates()
class(Data.df) = "data.frame"
Data.df = AddZeroValueColumns(Data.df)
Data.df = TidyData(Data.df)

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

#test = spfkm(FullFormula, Data.df["dominant_lc"], Data.df[,GetAllPixelCovars()])

registerDoParallel(cores=10)
fnc_wm_cv = CrossValidate(FullFormula, Data.df, train_wm.spfkm, predict.spfkm, pred_formula=FullFormula, covariate_names=c(GetAllPixelCovars(), "dominant_lc"), packages=c("GSIF", "formula.tools"))
AccuracyStatisticsPlots(fnc_wm_cv, Data.df[, colnames(fnc_wm_cv)]/100) # RMSE of 22%
SCM(fnc_wm_cv, as.matrix(Data.df[, colnames(fnc_wm_cv)]/100), plot=TRUE, totals=TRUE, scale=TRUE) # 49% accuracy, kappa 0.41: pretty poor

# with logistic regression to estimate centroids: doesn't work due to small classes
fnc_lr_cv = CrossValidate(FullFormula, Data.df, train_lr.spfkm, predict.spfkm, pred_formula=FullFormula, covariate_names=c(GetAllPixelCovars(), "dominant_lc"), packages=c("GSIF", "formula.tools"))
AccuracyStatisticsPlots(fnc_lr_cv, Data.df[, colnames(fnc_lr_cv)]/100) # RMSE of 22%
SCM(fnc_lr_cv, as.matrix(Data.df[, colnames(fnc_lr_cv)]/100), plot=TRUE, totals=TRUE, scale=TRUE)

# Crisp
fnc_wm_cv = CrossValidate(FullFormula, Data.df, train_wm.spfkm, predict.spfkm, pred_formula=FullFormula, covariate_names=c(GetAllPixelCovars(), "dominant_lc"), packages=c("GSIF", "formula.tools"), fuzzy.e=1.01)
AccuracyStatisticsPlots(fnc_wm_cv, Data.df[, colnames(fnc_wm_cv)]/100) # RMSE of 29%
SCM(fnc_wm_cv, as.matrix(Data.df[, colnames(fnc_wm_cv)]/100), plot=TRUE, totals=TRUE, scale=TRUE) # 46% accuracy, kappa 0.37: worse

# Use uncorrelated variables
UncorFormula = formula(paste("dominant_lc", paste0(GetUncorrelatedPixelCovars(), collapse="+"), sep="~"))
fnc_wm_cv = CrossValidate(UncorFormula, Data.df, train_wm.spfkm, predict.spfkm, pred_formula=UncorFormula, covariate_names=c(GetAllPixelCovars(), "dominant_lc"), packages=c("GSIF", "formula.tools"))
AccuracyStatisticsPlots(fnc_wm_cv, Data.df[, colnames(fnc_wm_cv)]/100) # RMSE of 21%
SCM(fnc_wm_cv, as.matrix(Data.df[, colnames(fnc_wm_cv)]/100), plot=TRUE, totals=TRUE, scale=TRUE) # 47% accuracy, kappa 0.38: pretty poor

# Log-transform slope, amplitudes, nir
Data.trans = Data.df
Data.trans$slope = log(Data.trans$slope+0.000001)
Data.trans$nir = log(Data.trans$nir)
Data.trans$amplitude1 = log(Data.trans$amplitude1+0.000001)
Data.trans$amplitude2 = log(Data.trans$amplitude2+0.000001)

UncorFormula = formula(paste("dominant_lc", paste0(GetUncorrelatedPixelCovars(), collapse="+"), sep="~"))
fnc_wm_cv = CrossValidate(UncorFormula, Data.trans, train_wm.spfkm, predict.spfkm, pred_formula=UncorFormula, covariate_names=c(GetAllPixelCovars(), "dominant_lc"), packages=c("GSIF", "formula.tools"))
AccuracyStatisticsPlots(fnc_wm_cv, Data.df[, colnames(fnc_wm_cv)]/100) # RMSE of 22%
SCM(fnc_wm_cv, as.matrix(Data.df[, colnames(fnc_wm_cv)]/100), plot=TRUE, totals=TRUE, scale=TRUE) # 46% accuracy, kappa 0.37: worse

# Scale
Data.scaled = apply(Data.df[,GetUncorrelatedPixelCovars()], 2, scale)
Data.scaled = cbind(Data.scaled, Data.df[,!names(Data.df) %in% GetUncorrelatedPixelCovars()])
fnc_wm_cv = CrossValidate(UncorFormula, Data.scaled, train_wm.spfkm, predict.spfkm, pred_formula=UncorFormula, covariate_names=c(GetAllPixelCovars(), "dominant_lc"), packages=c("GSIF", "formula.tools"))
AccuracyStatisticsPlots(fnc_wm_cv, Data.df[, colnames(fnc_wm_cv)]/100) # RMSE of 21%
SCM(fnc_wm_cv, as.matrix(Data.df[, colnames(fnc_wm_cv)]/100), plot=TRUE, totals=TRUE, scale=TRUE) # 47% accuracy, kappa 0.38: no difference

# What is the difference between centroids derived from logistic regression and weighted average
LogCentroids = spmultinom(formulaString=FullFormula, Data.df["dominant_lc"], Data.df[,GetAllPixelCovars()], class.stats=TRUE, predict.probs=FALSE)
LogCentroids$class.c
SimpleCentroids = GetClassMeans(FullFormula, Data.df)
CentroidDifferences = LogCentroids$class.c - SimpleCentroids
AccuracyStats(LogCentroids$class.c, SimpleCentroids)

LogCentroids$class.sd
SimpleCentroidSDs = GetClassSDs(FullFormula, Data.df)
SDDifferences = LogCentroids$class.sd - SimpleCentroidSDs
AccuracyStats(LogCentroids$class.sd, SimpleCentroidSDs)

# How good is it to use weighted means
WMModel = spfkm(FullFormula, Data.df["dominant_lc"], Data.df[,GetAllPixelCovars()],
    class.c=GetClassMeans(FullFormula, Data.df), class.sd=GetClassSDs(FullFormula, Data.df))

AccuracyStatisticsPlots(WMModel@mu, Data.df[, colnames(WMModel@mu)]/100) # RMSE of 22%
SCM(WMModel@mu, as.matrix(Data.df[, colnames(WMModel@mu)]/100), plot=TRUE, totals=TRUE, scale=TRUE) # 49% accuracy, kappa 0.41: pretty poor still but a bit better (and much faster)

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
