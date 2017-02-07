# Classification using Random Forest regression, one model per variable
library(ranger)
library(caret)
library(raster)
source("utils/set-temp-path.r")
source("utils/load-data.r")
source("utils/accuracy-statistics.r")

alldata = LoadClassificationData()

set.seed(0xbadcafe)
folds = createFolds(alldata$cropland, 2)
fold = folds$Fold1

Formula = paste0("water~", paste(TrainingNames, collapse = "+"))
Formula = update.formula(Formula, "~.-ndvi")
rf.water = ranger(Formula, alldata[fold,]@data)
rf.water$variable.importance
predicted.water = predict(rf.water, alldata[fold,]@data)
AccuracyStats(predicted.water$predictions, alldata[-fold,]@data$water)

rfh.water = holdoutRF(Formula, alldata@data)
rfh.water$variable.importance
predictedh.water = predict(rfh.water$rf1, alldata@data)
# RMSE=11.0 (without ts-cleaning: 11.5). Amazing. Better than 27.
AccuracyStats(predictedh.water$predictions, alldata@data$water)

# Create a full prediction table
FullFormula = paste0("~", paste(TrainingNames, collapse = "+"))
FullFormula = update.formula(FullFormula, "~.-ndvi")
Predictions = matrix(ncol=length(ValidationNames), nrow=nrow(alldata@data),
    dimnames=list(list(), ValidationNames))
for (Class in ValidationNames)
{
    print(Class)
    Formula = update.formula(FullFormula, paste0(Class, " ~ ."))
    set.seed(0xbadcafe)
    rfmodel = holdoutRF(Formula, alldata@data)
    #print(rfmodel$variable.importance)
    #barplot(rfmodel$variable.importance, main=Class)
    rfprediction = predict(rfmodel$rf1, alldata@data)
    # RMSE=20-10. Amazing. Better than 27.
    #print(AccuracyStats(rfprediction$predictions, alldata@data[,Class]))
    Predictions[,Class] = rfprediction$predictions
}
AccuracyStatTable(Predictions, alldata@data[,ValidationNames])

# Scale prediction table
ScaledPredictions = Predictions
for (i in 1:nrow(Predictions))
{
    ScaledPredictions[i,] = Predictions[i,] / sum(Predictions[i,]) * 100
}
# 17.8 overall
AccuracyStatTable(ScaledPredictions, alldata@data[,ValidationNames])


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
