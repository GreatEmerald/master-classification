# Predict pixels using Random Forest

library(ranger)
library(caret)
source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("pixel-based/utils/crossvalidation.r")
source("pixel-based/utils/subpixel-confusion-matrix.r")
source("utils/accuracy-statistics.r")

Data.df = LoadTrainingAndCovariates()
class(Data.df) = "data.frame"
Data.df = AddZeroValueColumns(Data.df)
Data.df = TidyData(Data.df)

# Make sure there are no NAs left
apply(Data.df, 2, function(x){sum(is.na(x))}) / nrow(Data.df) * 100

# Do some cross-validation

set.seed(0xfedbeef)
#folds = createFolds(Data.df$location_id, 10)
# Use stratified random sampling: make sure that we validate using all classes. Due to a large dataset, this hardly matters, but hey.
folds = createFolds(Data.df$dominant_lc, 10)
Classes = GetLargeClassNames(Data.df)
Truth = Data.df[,Classes]

# We have zero- and 100-inflation in the data.
# If we adjust for the zero inflation and use zero-truncated data for the second model, it tends to just predict 100.
# If we use both zero- and 100-truncated data, it still tends to 100, but is more fuzzy.
RFCV = function(outdir, filename, InflationAdjustment=1, TruncateZeroes = FALSE, scale=TRUE, ...)
{
    OutputFile = file.path(outdir, paste0("predictions-", filename))
    if (file.exists(OutputFile))
        return(read.csv(OutputFile))
        
    Covariates = GetAllPixelCovars()#GetUncorrelatedPixelCovars()
    FullFormula = paste0("~", paste(Covariates, collapse = "+"))
    PredictionsPerFold = data.frame()
    for (i in 1:length(folds))
    {
        TrainingSet = Data.df[-folds[[i]],]
        #TrainingSet = Oversample(TrainingSet)
        ValidationSet = Data.df[folds[[i]],]
        
        Predictions = matrix(ncol=length(Classes), nrow=length(folds[[i]]), dimnames=list(list(), Classes))
        for (Class in Classes)
        {
            print(Class)
            ZeroClass = paste("no", Class, sep=".")
            
            Formula = update.formula(FullFormula, paste0(Class, " ~ ."))
            if (InflationAdjustment > 0)
            {
                if (InflationAdjustment == 1) # Zero-inflation only
                {
                    # Predict zeroes
                    ZeroFormula = update.formula(FullFormula, paste0("as.factor(", ZeroClass, ") ~ ."))
                    ZeroModel = ranger(ZeroFormula, TrainingSet, seed = 0xbadcafe)
                    ClassPredictions = predict(ZeroModel, ValidationSet)
                    ClassPredictions = as.numeric(!as.logical(ClassPredictions$predictions))
                    NonZeroes = ClassPredictions==1
                } else if (InflationAdjustment == 2) # Zero and 100 inflation
                {
                    # Convert the "no." column to a factor, "zero", "hundred", "in-between"
                    TrainingCategories = rep("in-between", nrow(TrainingSet))
                    TrainingCategories[TrainingSet[,Class] == 0] = "zero"
                    TrainingCategories[TrainingSet[,Class] == 100] = "hundred"
                    TrainingCategories = factor(TrainingCategories)
                    TrainingSet[,ZeroClass] = TrainingCategories
                    
                    ZeroFormula = update.formula(FullFormula, paste0(ZeroClass, " ~ ."))
                    ZeroModel = ranger(ZeroFormula, TrainingSet, seed = 0xbadcafe)
                    
                    CategoryPredictions = predict(ZeroModel, ValidationSet)$prediction
                    ClassPredictions = as.numeric(CategoryPredictions) # For length
                    ClassPredictions[CategoryPredictions == "zero"] = 0
                    ClassPredictions[CategoryPredictions == "hundred"] = 100
                    NonZeroes = CategoryPredictions=="in-between"
                }
                
                # Predict non-zeroes
                if (any(NonZeroes))
                {
                    # Whether to use all data for training, or zero-truncate. Truncating makes the model biased towards 100...
                    if (TruncateZeroes)
                    {
                        if (InflationAdjustment == 1)
                            NonzeroModel = ranger(Formula, TrainingSet[TrainingSet[,Class] > 0,], seed = 0xbadcafe)
                        else if (InflationAdjustment == 2)
                            NonzeroModel = ranger(Formula, TrainingSet[TrainingSet[,Class] > 0 & TrainingSet[,Class] < 100,], seed = 0xbadcafe)
                    }
                    else
                        NonzeroModel = ranger(Formula, TrainingSet, seed = 0xbadcafe)
                    ClassPredictions[NonZeroes] = predict(NonzeroModel, ValidationSet[NonZeroes,])$prediction
                } else print("Everything was predicted to be zero!")
            } else {
                # Predict all
                rfmodel = ranger(Formula, TrainingSet, seed = 0xbadcafe)
                ClassPredictions = predict(rfmodel, ValidationSet)$prediction
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
    PredictionsPerFold = PredictionsPerFold[order(unlist(folds)),]
    write.csv(PredictionsPerFold, OutputFile, row.names=FALSE)
    return(PredictionsPerFold)
}

# Actually, oversampling in this case is not needed, because we have a model per class (or two). Y is not unbalanced in that case, just zero-inflated (to various degrees).
# TODO: see if this can be rewritten using CrossValidate()

PredictionResult = RFCV("../data/pixel-based/predictions/", "randomforest-onestep-allcovars-10folds.csv", InflationAdjustment = 0)
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 16%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 66%, kappa 0.57

PredictionResult = RFCV("../data/pixel-based/predictions/", "randomforest-twostep-truncated-allcovars-10folds.csv", InflationAdjustment = 1, TruncateZeroes = TRUE)
PredictionResult[rowSums(PredictionResult) == 0,] = rep(10,10) # Set cases of all 0 to all 10
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 17%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 71±2%, kappa 0.63 - this is much better

PredictionResult = RFCV("../data/pixel-based/predictions/", "randomforest-threestep-truncated-allcovars-10folds.csv", InflationAdjustment = 2, TruncateZeroes = TRUE)
PredictionResult[rowSums(PredictionResult) == 0,] = rep(10,10) # Set cases of all 0 to all 10
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 17%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 70%, kappa 0.62 - this doesn't help any

# What if we don't truncate
PredictionResult = RFCV("../data/pixel-based/predictions/", "randomforest-twostep-untruncated-allcovars-10folds.csv", InflationAdjustment = 1, TruncateZeroes = FALSE)
PredictionResult[rowSums(PredictionResult) == 0,] = rep(10,10) # Set cases of all 0 to all 10
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 17%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 71%, kappa 0.63 - so truncation doesn't matter

PredictionResult = RFCV("../data/pixel-based/predictions/", "randomforest-threestep-untruncated-allcovars-10folds.csv", InflationAdjustment = 2, TruncateZeroes = FALSE)
PredictionResult[rowSums(PredictionResult) == 0,] = rep(10,10) # Set cases of all 0 to all 10
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 17%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 70%, kappa 0.62 - so truncation doesn't matter here either


AST = AccuracyStatTable(PredictionResult[,Classes], Truth[,Classes])
print(AST)
barplot(AST$RMSE, names.arg=rownames(AST), main="RMSE")
barplot(AST$MAE, names.arg=rownames(AST), main="MAE")
barplot(AST$ME, names.arg=rownames(AST), main="ME")
#plot(unlist(PredictionsPerFold), unlist(Truth))
#abline(0, 1, col="red")
write.csv(AST, paste0("../data/pixel-based/predictions/", "randomforest-twostep-truncated-allcovars-10folds.csv"))

# ggplot for more reasonable display of ludicrous amounts of points
ggplot(data.frame(Prediction=unlist(PredictionResult), Truth=unlist(Truth)), aes(Prediction, Truth)) +
    geom_hex() +
    scale_fill_distiller(palette=7, trans="log") + #log scale
    geom_abline(slope=1, intercept=0) + ggtitle("Random Forest, 10-fold CV, two models, zeroes truncated")
    
