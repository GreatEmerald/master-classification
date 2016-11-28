# Classification using Random Forest regression, one model per variable
library(ranger)
library(caret)
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
# RMSE=11.5. Amazing. Better than 27.
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
AccuracyStatTable(ScaledPredictions, alldata@data[,ValidationNames])

