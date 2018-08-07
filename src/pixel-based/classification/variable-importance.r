# Script for getting variable importance using Random Forest
library(ranger)
library(caret)

source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")

Data.df = LoadTrainingAndCovariates()
class(Data.df) = "data.frame"
Data.df = AddZeroValueColumns(Data.df)

Data.df = Data.df[!is.na(Data.df$phase2),]
Data.df = Data.df[!is.na(Data.df$nir),]
Data.df = Data.df[!is.na(Data.df$slope),]

set.seed(0xbadcafe)
folds = createFolds(Data.df$location_id, 4)

ClassNames = GetIIASAClassNames()
UncorrelatedCovars = GetUncorrelatedPixelCovars()

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
    bigplot=c(0.25, 0.85, 0.25, 0.95), smallplot=c(0.88, 0.9, 0.25, 0.95))
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
    if ((Importances[i,j] > z.yellows[1]) & (Importances[i,j] < z.yellows[2]))
    {
      text(i,j,round(Importances[i,j]), col="black", cex = 0.8)
    }else
    {
      text(i,j,round(Importances[i,j]), col="white", cex = 0.8)     
    }
  }
}
