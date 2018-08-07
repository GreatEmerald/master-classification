# Predict pixels using Random Forest

library(ranger)
library(caret)
source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("utils/accuracy-statistics.r")

Data.df = LoadTrainingAndCovariates()
class(Data.df) = "data.frame"
Data.df = AddZeroValueColumns(Data.df)

# Filter out data with NAs:
nrow(Data.df) # 31964
# No harmonics
Data.df = Data.df[!is.na(Data.df$phase2),]
# And all values that don't have observations in 2017
Data.df = Data.df[!is.na(Data.df$nir),]
# And all data that is outside Africa (we didn't process terrain there)
Data.df = Data.df[!is.na(Data.df$slope),]
nrow(Data.df) # 26925
# Make sure there are no NAs
apply(Data.df, 2, function(x){sum(is.na(x))}) / nrow(Data.df) * 100

# Do some cross-validation

set.seed(0xfedbeef)
folds = createFolds(Data.df$location_id, 2)
Classes = GetIIASAClassNames()
Truth = Data.df[,Classes]

# We have zero- and 100-inflation in the data.
# If we adjust for the zero inflation and use zero-truncated data for the second model, it tends to just predict 100.
# If we use both zero- and 100-truncated data, it still tends to 100, but is more fuzzy.
RFCV = function(outdir, filename, ZeroInflated = TRUE, scale=TRUE, ...)
{
    OutputFile = file.path(outdir, paste0("predictions-", filename))
    if (file.exists(OutputFile))
        return(read.csv(OutputFile))
        
    Covariates = GetUncorrelatedPixelCovars()
    FullFormula = paste0("~", paste(Covariates, collapse = "+"))
    PredictionsPerFold = data.frame()
    for (i in 1:length(folds))
    {
        TrainingSet = Data.df[-folds[[i]],]
        ValidationSet = Data.df[folds[[i]],]
        
        Predictions = matrix(ncol=length(Classes), nrow=length(folds[[i]]), dimnames=list(list(), Classes))
        for (Class in Classes)
        {
            print(Class)
            ZeroClass = paste("no", Class, sep=".")
            
            Formula = update.formula(FullFormula, paste0(Class, " ~ ."))
            if (ZeroInflated)
            {
                # Predict zeroes
                ZeroFormula = update.formula(FullFormula, paste0("as.factor(", ZeroClass, ") ~ ."))
                ZeroModel = ranger(ZeroFormula, TrainingSet, seed = 0xbadcafe)
                ClassPredictions = predict(ZeroModel, ValidationSet)
                ClassPredictions = as.numeric(!as.logical(ClassPredictions$predictions))
                NonZeroes = ClassPredictions==1
                
                # Predict non-zeroes
                if (any(NonZeroes))
                {
                    NonzeroModel = ranger(Formula, TrainingSet[!TrainingSet[,ZeroClass],], seed = 0xbadcafe)
                    ClassPredictions[NonZeroes] = predict(NonzeroModel, ValidationSet[NonZeroes,])$prediction
                } else print("Everything was predicted to be zero!")
            } else {
                # Predict all
                rfmodel = ranger(Formula, TrainingSet, seed = 0xbadcafe)
                ClassPredictions = predict(rfmodel, ValidationSet)
            }
            
            Predictions[,Class] = ClassPredictions
        }
        if (scale)
        {
            Predictions = Predictions / rowSums(Predictions) * 100
            # There is a possibility that all classes have been predicted as 0, so we can't normalise.
            # In that case we just keep them as 0%. It won't add up to 100%. Alternatively we can set it to 1/nclass.
            Predictions[is.nan(Predictions)] = 0
        }
        PredictionsPerFold = rbind(PredictionsPerFold, Predictions)
    }
    # Sort everything back to the order of the original
    write.csv(PredictionsPerFold[order(unlist(folds)),], OutputFile, row.names=FALSE)
    return(PredictionsPerFold)
}

PredictionResult = RFCV("../data/pixel-based/predictions/", "randomforest-twostep-uncorrelated-unscaled-2.csv", scale=FALSE)

AST = AccuracyStatTable(PredictionResult, Truth)
print(AST)
barplot(AST$RMSE, names.arg=rownames(AST), main="RMSE")
barplot(AST$MAE, names.arg=rownames(AST), main="MAE")
barplot(AST$ME, names.arg=rownames(AST), main="ME")
#plot(unlist(PredictionsPerFold), unlist(Truth))
#abline(0, 1, col="red")
write.csv(AST, paste0("../data/pixel-based/predictions/", "randomforest-twostep-uncorrelated.csv"))

# ggplot for more reasonable display of ludicrous amounts of points
ggplot(data.frame(Prediction=unlist(PredictionResult), Truth=Truth), aes(Prediction, Truth)) +
    geom_hex() +
    scale_fill_viridis_c(trans="log") #log scale