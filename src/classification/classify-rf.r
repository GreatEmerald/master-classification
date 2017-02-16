# Classification using Random Forest regression, one model per variable
library(ranger)
library(caret)
library(raster)
library(fields)
source("utils/set-temp-path.r")
source("utils/load-data.r")
source("utils/accuracy-statistics.r")

alldata = LoadClassificationData()

set.seed(0xbadcafe)
folds = createFolds(alldata$cropland, 4)

TN = GetTrainingNames()#exclude=c("osavi", "aspect", "is.water", "height"))

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

image.plot(1:length(GetValidationNames()), 1:length(TN), as.matrix(Importances),
    axes=FALSE, xlab = "Class", ylab = "Covariate")
points(0,0)
axis(side=1, at=1:length(GetValidationNames()), labels=row.names(Importances), las=2)
axis(side=2, at=1:length(TN), labels=names(Importances), las=1)


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

# Do 4-fold cross-validation
FullFormula = paste0("~", paste(TN, collapse = "+"))
ASTs = data.frame()
for (i in 1:length(folds))
{
    Predictions = matrix(ncol=length(GetValidationNames()), nrow=nrow(alldata@data[folds[[i]],]),
        dimnames=list(list(), GetValidationNames()))
    for (Class in GetValidationNames())
    {
        print(Class)
        Formula = update.formula(FullFormula, paste0(Class, " ~ ."))
        rfmodel = ranger(Formula, alldata@data[-folds[[i]],], seed = 0xbadcafe)
        rfprediction = predict(rfmodel, alldata@data[folds[[i]],])
        Predictions[,Class] = rfprediction$predictions
    }
    ScaledPredictions = Predictions
    for (n in 1:nrow(Predictions))
    {
        ScaledPredictions[n,] = Predictions[n,] / sum(Predictions[n,]) * 100
    }
    AST = AccuracyStatTable(ScaledPredictions, alldata@data[folds[[i]],GetValidationNames()])
    AST$class = factor(rownames(AST))
    if (nrow(ASTs) == 0)
        ASTs = AST
    else
        ASTs = rbind(ASTs, AST)
}
CVAST = stats::aggregate(ASTs[,-(which(names(ASTs) == "class"))], by=list(class=ASTs$class), FUN=mean)
CVAST[match(AST$class, CVAST$class),]

# Try making a full-raster prediction
TrainingRasters = stack(TrainingFiles)
names(TrainingRasters) = TrainingNames

Formula = paste0("water~", paste(TrainingNames, collapse = "+"))
Formula = update.formula(Formula, "~.-ndvi")
rf.water = ranger(Formula, alldata@data)
# Predict a block of 1/5 of the data that does not have NAs (two pixels at the edge have NAs)
predicted.water = predict(rf.water, getValuesBlock(TrainingRasters, 2016*4, 2014, 2016, 2016), num.threads=31)
# Recreate a raster
pred.matrix = matrix(predicted.water$predictions, nrow=2014, ncol=2016, byrow=TRUE)
pred.raster = raster(pred.matrix, crs=crs(TrainingRasters),
    xmn=extent(TrainingRasters)@xmin + (extent(TrainingRasters)@xmax - extent(TrainingRasters)@xmin) / 5 * 1,
    ymn=extent(TrainingRasters)@ymin +
        2 * (extent(TrainingRasters)@ymax - extent(TrainingRasters)@ymin) / dim(TrainingRasters)[2],
    xmx=extent(TrainingRasters)@xmin + (extent(TrainingRasters)@xmax - extent(TrainingRasters)@xmin) / 5 * 2,
    ymx=extent(TrainingRasters)@ymax - (extent(TrainingRasters)@ymax - extent(TrainingRasters)@ymin) / 5 * 4)
spplot(pred.raster)
pred.raster = writeRaster(pred.raster, filename="../../userdata/waterpredictiontemp.tif", overwrite=TRUE)
