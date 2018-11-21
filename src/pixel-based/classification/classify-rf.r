# Predict pixels using Random Forest

library(ranger)
#library(caret)
source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
#source("pixel-based/utils/crossvalidation.r")
source("pixel-based/utils/subpixel-confusion-matrix.r")
source("utils/accuracy-statistics.r")

Data.df = LoadTrainingAndCovariates()
class(Data.df) = "data.frame"
Data.df = AddZeroValueColumns(Data.df)
Data.df = TidyData(Data.df)

# Make sure there are no NAs left
apply(Data.df[,GetAllPixelCovars()], 2, function(x){sum(is.na(x))}) / nrow(Data.df) * 100

# Validation data
Data.val = LoadValidationAndCovariates()
class(Data.val) = "data.frame"
Data.val = TidyData(Data.val) # Drops around 450

apply(Data.val, 2, function(x){sum(is.na(x))}) / nrow(Data.df) * 100

# Do some cross-validation

#set.seed(0xfedbeef)
#folds = createFolds(Data.df$location_id, 10)
# Use stratified random sampling: make sure that we validate using all classes. Due to a large dataset, this hardly matters, but hey.
#folds = createFolds(Data.df$dominant_lc, 10)
Classes = GetCommonClassNames()
Truth = Data.val[,Classes]

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

# No CV
RFTrain = function(outdir, filename, InflationAdjustment=1, TruncateZeroes = FALSE, scale=TRUE, ...)
{
    OutputFile = file.path(outdir, paste0("predictions-", filename))
    if (file.exists(OutputFile))
        return(read.csv(OutputFile))
    
    Covariates = GetAllPixelCovars()#GetUncorrelatedPixelCovars()
    FullFormula = paste0("~", paste(Covariates, collapse = "+"))
    TrainingSet = Data.df
    #TrainingSet = Oversample(TrainingSet)
    ValidationSet = Data.val
        
    Predictions = matrix(ncol=length(Classes), nrow=nrow(ValidationSet), dimnames=list(list(), Classes))
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
                ZeroModel = ranger(ZeroFormula, TrainingSet, seed = 0xbadcafe, ...)
                    
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
                        NonzeroModel = ranger(Formula, TrainingSet[TrainingSet[,Class] > 0,], seed = 0xbadcafe, ...)
                    else if (InflationAdjustment == 2)
                        NonzeroModel = ranger(Formula, TrainingSet[TrainingSet[,Class] > 0 & TrainingSet[,Class] < 100,], seed = 0xbadcafe, ...)
                } else
                    NonzeroModel = ranger(Formula, TrainingSet, seed = 0xbadcafe, ...)
                ClassPredictions[NonZeroes] = predict(NonzeroModel, ValidationSet[NonZeroes,])$prediction
            } else print("Everything was predicted to be zero!")
        } else {
            # Predict all
            rfmodel = ranger(Formula, TrainingSet, seed = 0xbadcafe, ...)
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
    
    write.csv(Predictions, OutputFile, row.names=FALSE)
    return(as.data.frame(Predictions))
}

# Actually, oversampling in this case is not needed, because we have a model per class (or two). Y is not unbalanced in that case, just zero-inflated (to various degrees).
# TODO: see if this can be rewritten using CrossValidate()

PredictionResult = RFCV("../data/pixel-based/predictions/", "randomforest-onestep-allcovars-10folds.csv", InflationAdjustment = 0)
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 16%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 66%, kappa 0.57
cor(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100))^2 # 0.65

# Holdout validation
PredictionResult = RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-allcovars-validation.csv", InflationAdjustment = 0, mtry=20)
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 18%, still pretty good
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 64%, kappa 0.57
cor(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100))^2 # 0.58

PredictionResult = RFCV("../data/pixel-based/predictions/", "randomforest-twostep-truncated-allcovars-10folds.csv", InflationAdjustment = 1, TruncateZeroes = TRUE)
PredictionResult[rowSums(PredictionResult) == 0,] = rep(10,10) # Set cases of all 0 to all 10
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 17%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 71±2%, kappa 0.63 - this is much better
cor(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100))^2 # 0.60

# Holdout
PredictionResult = RFTrain("../data/pixel-based/predictions/", "randomforest-twostep-truncated-allcovars-validation.csv", InflationAdjustment = 1, TruncateZeroes = TRUE)
PredictionResult[rowSums(PredictionResult) == 0,] = rep(100/length(Classes),length(Classes)) # Set cases of all 0 to equal
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 20%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 68±3%, kappa 0.60 - this is much better
cor(c(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100))^2 # 0.52

PredictionResult = RFCV("../data/pixel-based/predictions/", "randomforest-threestep-truncated-allcovars-10folds.csv", InflationAdjustment = 2, TruncateZeroes = TRUE)
PredictionResult[rowSums(PredictionResult) == 0,] = rep(10,10) # Set cases of all 0 to all 10
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 17%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 70%, kappa 0.62 - this doesn't help any
cor(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100))^2 # 0.58

# What if we don't truncate
PredictionResult = RFCV("../data/pixel-based/predictions/", "randomforest-twostep-untruncated-allcovars-10folds.csv", InflationAdjustment = 1, TruncateZeroes = FALSE)
PredictionResult[rowSums(PredictionResult) == 0,] = rep(10,10) # Set cases of all 0 to all 10
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 17%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 71%, kappa 0.63 - so truncation doesn't matter
cor(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100))^2 # 0.60

PredictionResult = RFCV("../data/pixel-based/predictions/", "randomforest-threestep-untruncated-allcovars-10folds.csv", InflationAdjustment = 2, TruncateZeroes = FALSE)
PredictionResult[rowSums(PredictionResult) == 0,] = rep(10,10) # Set cases of all 0 to all 10
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 17%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 70%, kappa 0.62 - so truncation doesn't matter here either
cor(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100))^2 # 0.59

AST = AccuracyStatTable(PredictionResult[,Classes], Truth[,Classes])
print(AST)
barplot(AST$RMSE, names.arg=rownames(AST), main="RMSE")
barplot(AST$MAE, names.arg=rownames(AST), main="MAE")
barplot(AST$ME, names.arg=rownames(AST), main="ME")
#plot(unlist(PredictionsPerFold), unlist(Truth))
#abline(0, 1, col="red")
write.csv(AST, paste0("../data/pixel-based/predictions/", "randomforest-twostep-truncated-allcovars-10folds.csv"))

library(ggplot2)
# ggplot for more reasonable display of ludicrous amounts of points
ggplot(data.frame(Prediction=unlist(PredictionResult), Truth=unlist(Truth)), aes(Prediction, Truth)) +
    geom_hex() +
    scale_fill_distiller(palette=7, trans="log") + #log scale
    geom_abline(slope=1, intercept=0) + ggtitle("Random Forest, Validation, single model")
    
