# Script for getting variable importance using Random Forest
library(ranger)
library(caret)

source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("pixel-based/utils/crossvalidation.r")

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

Truth = Data.val[,GetCommonClassNames()]

# Get and plot variable importance
FullFormula = paste0("~", paste(UncorrelatedCovars, collapse = "+"))
Importances = data.frame()
for (Class in ClassNames)
{
    print(Class)
    Formula = update.formula(FullFormula, paste0(Class, " ~ ."))
    set.seed(0xbadcafe)
    rfmodel = holdoutRF(Formula, Data.df, scale.permutation.importance=TRUE)
    Importances = rbind(Importances, rfmodel$variable.importance)
    names(Importances) = names(rfmodel$variable.importance)
    row.names(Importances)[nrow(Importances)] = Class
}

sort(colSums(Importances))


fields::image.plot(1:length(ClassNames), 1:length(UncorrelatedCovars), as.matrix(Importances),
    axes=FALSE, xlab = "", ylab = "", #xlab = "Class", ylab = "Covariate")
    bigplot=c(0.25, 0.85, 0.25, 0.95), smallplot=c(0.88, 0.9, 0.25, 0.95), col=fields::designer.colors(1000, c("white", "darkred")))
#points(0,0)
axis(side=1, at=1:length(ClassNames), labels=row.names(Importances), las=2)
axis(side=2, at=1:length(UncorrelatedCovars), labels=names(Importances), las=1)


min.z <- min(Importances)
max.z <- max(Importances)
z.yellows <- min.z + (max.z - min.z)/64*c(20,45) 
# Loop for label printing: black letters on light background and white letters on dark background
for (i in 1:length(ClassNames))
{
  for (j in 1:length(UncorrelatedCovars))
  {
    #if ((Importances[i,j] > z.yellows[1]) & (Importances[i,j] < z.yellows[2]))
    #{
      text(i,j,round(Importances[i,j]), col="black", cex = 0.8)
    #}else
    #{
    #  text(i,j,round(Importances[i,j]), col="white", cex = 0.8)     
    #}
  }
}

## Group covariates by type of information they provide

PermuteMe = function(CovarsInGroup, Model, Class, RealPrediction, statistic="RMSE")
{
    PermutedVal = Data.val
    set.seed(0xbeefcab)
    for (covar in CovarsInGroup)
        PermutedVal[[covar]] = sample(PermutedVal[[covar]])
        
    Permutation = predict(Model, PermutedVal)
    AccuracyStats(Permutation$predictions, Truth[[Class]])[[statistic]] - AccuracyStats(RealPrediction$predictions, Truth[[Class]])[[statistic]]
}

GetPermutationImportance = function(statistic="RMSE", AdjustPerNumCovars = FALSE, ClassNames=GetCommonClassNames(), CovarGroups=GetAllPixelCovars(TRUE))
{
    FullFormula = paste0("~", paste(GetAllPixelCovars(), collapse = "+"))
    Importances = NULL
    for (Class in ClassNames)
    {
        print(Class)
        Formula = update.formula(FullFormula, paste0(Class, " ~ ."))
        rfmodel = ranger(Formula, Data.df)
        RealPrediction = predict(rfmodel, Data.val)

        Importances = rbind(Importances, sapply(CovarGroups, PermuteMe, Model=rfmodel, Class=Class, RealPrediction=RealPrediction, statistic=statistic))
        row.names(Importances)[nrow(Importances)] = Class
    }
    if (AdjustPerNumCovars)
        return(t(t(Importances) / sapply(CovarGroups, length)))
    return(Importances)
}

plot_heatmap = function(Importances)
{
    fields::image.plot(1:nrow(Importances), 1:ncol(Importances), Importances,
        axes=FALSE, xlab = "", ylab = "", #xlab = "Class", ylab = "Covariate")
        bigplot=c(0.25, 0.85, 0.25, 0.95), smallplot=c(0.88, 0.9, 0.25, 0.95), col=fields::designer.colors(100, c("white", "darkred")))
    #points(0,0)
    axis(side=1, at=1:nrow(Importances), labels=rownames(Importances), las=2)
    axis(side=2, at=1:ncol(Importances), labels=colnames(Importances), las=1)


    #min.z <- min(Importances)
    #max.z <- max(Importances)
    #z.yellows <- min.z + (max.z - min.z)/64*c(20,45) 
    # Loop for label printing: black letters on light background and white letters on dark background
    for (i in 1:nrow(Importances))
    {
        for (j in 1:ncol(Importances))
        {
            #if ((Importances[i,j] > z.yellows[1]) & (Importances[i,j] < z.yellows[2]))
            #{
            text(i,j,round(Importances[i,j], 2), col="black", cex = 0.8)
            #}else
            #{
            #  text(i,j,round(Importances[i,j]), col="white", cex = 0.8)     
            #}
        }
    }
}

RawImportancesRMSE = GetPermutationImportance()
AdjustedImportancesRMSE = t(t(RawImportancesRMSE) / sapply(GetAllPixelCovars(TRUE), length))
svg("../rf-importances-rmse.svg", width = 12)
plot_heatmap(RawImportancesRMSE)
dev.off()
plot_heatmap(AdjustedImportancesRMSE)
RawImportancesMAE = GetPermutationImportance("MAE")
AdjustedImportancesMAE = t(t(RawImportancesMAE) / sapply(CovarGroups, length))
plot_heatmap(RawImportancesMAE)
plot_heatmap(AdjustedImportancesMAE)
