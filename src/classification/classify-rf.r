# Classification using Random Forest regression, one model per variable
library(ranger)
library(caret)
library(raster)
library(fields)
source("utils/set-temp-path.r")
source("utils/load-data.r")
source("utils/accuracy-statistics.r")

OutputDir = "../data/"

alldata = LoadClassificationData()
# Bare soil with NAs: 352 364 475
bsna = c(352, 364, 476)
#alldata@data = alldata@data[-bsna,]

set.seed(0xbadcafe)
folds = createFolds(alldata$cropland, 4)

TN = GetTrainingNames(exclude=c("osavi", "aspect", "is.water", "height"))

# Get and plot variable importance
FullFormula = paste0("~", paste(TN, collapse = "+"))
Importances = data.frame()
for (Class in GetValidationNames())
{
    print(Class)
    Formula = update.formula(FullFormula, paste0(Class, " ~ ."))
    set.seed(0xbadcafe)
    rfmodel = holdoutRF(Formula, alldata@data, scale.permutation.importance=TRUE)
    Importances = rbind(Importances, rfmodel$variable.importance)
    names(Importances) = names(rfmodel$variable.importance)
    row.names(Importances)[nrow(Importances)] = Class
}

sort(colSums(Importances))

PrettifyClasses = function(classnames)
{
    classnames = factor(classnames)
    levels(classnames) = list(
        "Total" = "Overall", "Crops" = "cropland", "Dec. trees" = "dec.trees", "Evgr. trees" = "evgr.trees",
        "Shrubs" = "shrubland", "Grassland" = "grassland", "Wetland" = "wetland", "Bare soil" = "bare.soil",
        "Urban" = "urban", "Water" = "water")
    return(classnames)
}

PrettifyCovariates = function(covarnames)
{
    covarnames = factor(covarnames)
    levels(covarnames) = list(
        "Red" = "red", "NIR" = "nir", "Blue" = "blue", "SWIR" = "swir", "LSWI" = "lswi",
        "Water mask" = "is.water", "Elevation" = "height", "Slope" = "slope", "Aspect" = "aspect",
        "TPI" = "tpi", "Mean NDVI" = "mean.ndvi", "Phase (1)" = "phase1", "Amplitude (1)" = "amplitude1",
        "Phase (2)" = "phase2", "Amplitude (2)" = "amplitude2")
    return(covarnames)
}

pdf("../plot/variable-importance.pdf", width=6, height=5)
image.plot(1:length(GetValidationNames()), 1:length(TN), as.matrix(Importances),
    axes=FALSE, xlab = "", ylab = "", #xlab = "Class", ylab = "Covariate")
    bigplot=c(0.25, 0.85, 0.25, 0.95), smallplot=c(0.88, 0.9, 0.25, 0.95))
points(0,0)
axis(side=1, at=1:length(GetValidationNames()), labels=PrettifyClasses(row.names(Importances)), las=2)
axis(side=2, at=1:length(TN), labels=PrettifyCovariates(names(Importances)), las=1)


min.z <- min(Importances)
max.z <- max(Importances)
z.yellows <- min.z + (max.z - min.z)/64*c(20,45) 
# print the labels
for(i in 1:length(GetValidationNames())){
  for(j in 1:length(TN)){
    if((Importances[i,j] > z.yellows[1])&(Importances[i,j] < z.yellows[2])){
      text(i,j,round(Importances[i,j]), col="black", cex = 0.8)
    }else{
      text(i,j,round(Importances[i,j]), col="white", cex = 0.8)     
    }
  }
}
dev.off()

# Do 4-fold cross-validation
RFCV = function(exclude=c(), filename=paste0(OutputDir, "stat-randomforest.csv"), ...)
{
    TN = GetTrainingNames(exclude=exclude)
    FullFormula = paste0("~", paste(TN, collapse = "+"))
    PredictionsPerFold = data.frame()
    for (i in 1:length(folds))
    {
        Predictions = matrix(ncol=length(GetValidationNames()), nrow=nrow(alldata@data[folds[[i]],]),
            dimnames=list(list(), GetValidationNames()))
        for (Class in GetValidationNames())
        {
            print(Class)
            Formula = update.formula(FullFormula, paste0(Class, " ~ ."))
            rfmodel = ranger(Formula, alldata@data[-folds[[i]],], seed = 0xbadcafe, ...)
            rfprediction = predict(rfmodel, alldata@data[folds[[i]],])
            Predictions[,Class] = rfprediction$predictions
        }
        ScaledPredictions = Predictions
        for (n in 1:nrow(Predictions))
        {
            ScaledPredictions[n,] = Predictions[n,] / sum(Predictions[n,]) * 100
        }
        if (nrow(PredictionsPerFold) == 0)
            PredictionsPerFold = ScaledPredictions
        else
            PredictionsPerFold = rbind(PredictionsPerFold, ScaledPredictions)
    }
    Validator = alldata@data[unlist(folds),GetValidationNames()]
    AST = AccuracyStatTable(PredictionsPerFold, Validator)
    print(AST)
    plot(unlist(PredictionsPerFold), unlist(Validator))
    write.csv(AST, filename)
}

# Unoptimised
RFCV(filename = paste0(OutputDir, "stat-randomforest-unoptimised.csv"))
# Optimised
RFCV(exclude=c("osavi", "aspect", "is.water", "height"), splitrule="maxstat", alpha=0.9, minprop=0.11)

#bs: 20.02770 with reduced dataset, 19.90767 without, so keeping it in is fine
# Full dataset: 21.01049
# Without OSAVI: 21.10414
# Without OSAVI and aspect: 21.02937
# Without 4 variables: 20.99727

# Train on pure, predict fuzzy
folds=list(which(!alldata@data$pure))
# Unoptimised
RFCV(filename = paste0(OutputDir, "stat-randomforest-pure-unoptimised.csv"))
# Optimised
RFCV(exclude=c("osavi", "aspect", "is.water", "height"), splitrule="maxstat", alpha=0.9, minprop=0.11,
    filename = paste0(OutputDir, "stat-randomforest-pure.csv"))
