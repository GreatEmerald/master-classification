# Predict pixels using Random Forest

library(ranger)
#library(caret)
source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("pixel-based/utils/crossvalidation.r")
source("pixel-based/utils/subpixel-confusion-matrix.r")
source("utils/accuracy-statistics.r")

Data.df = LoadTrainingAndCovariates()
Data.df = as.data.frame(Data.df)
Data.df = AddZeroValueColumns(Data.df)
Data.df = TidyData(Data.df)

# Make sure there are no NAs left
apply(Data.df[,GetAllPixelCovars()], 2, function(x){sum(is.na(x))}) / nrow(Data.df) * 100

# Validation data
Data.val = LoadValidationAndCovariates()
class(Data.val) = "data.frame"
Data.val = TidyData(Data.val) # Drops around 450 # Now drops ~800!

apply(Data.val, 2, function(x){sum(is.na(x))}) / nrow(Data.df) * 100

# Do some cross-validation

#set.seed(0xfedbeef)
#folds = createFolds(Data.df$location_id, 10)
# Use stratified random sampling: make sure that we validate using all classes. Due to a large dataset, this hardly matters, but hey.
#folds = createFolds(Data.df$dominant_lc, 10)
Classes = GetCommonClassNames()
Truth = Data.val[,Classes]
# Add column for purity
#Data.df$pure = apply(Data.df[,Classes], 1, max) > 95 # At 95%, half of our data is pure
#Data.val$pure = apply(Data.val[,Classes], 1, max) > 95 # At 95%, 42% of our validation is pure

# We have zero- and 100-inflation in the data.
# If we adjust for the zero inflation and use zero-truncated data for the second model, it tends to just predict 100.
# If we use both zero- and 100-truncated data, it still tends to 100, but is more fuzzy.
RFCV = function(outdir, filename, InflationAdjustment=1, TruncateZeroes = FALSE, scale=TRUE, covars=GetAllPixelCovars(), ...)
{
    OutputFile = file.path(outdir, paste0("predictions-", filename))
    if (file.exists(OutputFile))
        return(read.csv(OutputFile))
        
    Covariates = covars
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
RFTrain = function(outdir, filename, InflationAdjustment=1, TruncateZeroes = FALSE, scale=TRUE, covars=GetAllPixelCovars(), ...)
{
    OutputFile = file.path(outdir, paste0("predictions-", filename))
    if (file.exists(OutputFile))
        return(read.csv(OutputFile))
    
    Covariates = covars
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

# Three-step classification. More complex and doesn't share much code with the other approach, hence separate function.
RFTrain3 = function(outdir, filename, scale=TRUE, purity_threshold=95, covars=GetAllPixelCovars(), ...)
{
    OutputFile = file.path(outdir, paste0("predictions-threestep-", filename))
    if (file.exists(OutputFile))
        return(read.csv(OutputFile))
        
    Covariates = covars
    FullFormula = paste0("~", paste(Covariates, collapse = "+"))
    TrainingSet = Data.df
    #TrainingSet = Oversample(TrainingSet)
    TrainingSet$pure = apply(Data.df[,Classes], 1, max) > purity_threshold
    ValidationSet = Data.val
    ValidationSet$pure = apply(Data.val[,Classes], 1, max) > purity_threshold
    
    Predictions = matrix(ncol=length(Classes), nrow=nrow(ValidationSet), dimnames=list(list(), Classes))
    
    # Step one: pure/non-pure binary classification
    PureModel = ranger(paste0("as.factor(pure)", FullFormula), TrainingSet, seed = 0xbadcafe, ...)
    PureValPredictions = predict(PureModel, ValidationSet)
    PureValPredictions = as.logical(PureValPredictions$predictions)
    PurityAcc = mean(PureValPredictions == ValidationSet$pure)
    print(paste("Built purity classifier, accuracy:", PurityAcc))
    # We know whether the input is pure or not already, so for training we give the actual pure and nonpure pixels.
    # For predicting, we assume that PureModel is perfectly accurate.
    
    # Step two: classification of pure pixels
    ClassificationModel = ranger(paste0("dominant_lc", FullFormula), TrainingSet[TrainingSet$pure,], seed = 0xbadcafe, ...)
    ClassPredictions = predict(ClassificationModel, ValidationSet[PureValPredictions,]) # We use our pure model to select on which to predict
    # Expand into columns
    ClassCols = unclass(table(1:length(ClassPredictions$prediction),ClassPredictions$prediction)*100)
    Predictions[PureValPredictions, Classes] = ClassCols[,Classes]
    
    # Step three: regression of non-pure pixels, one model per class
    for (Class in Classes)
    {
        print(Class)
        Formula = update.formula(FullFormula, paste0(Class, " ~ ."))
        
        RegressionModel = ranger(Formula, TrainingSet[!TrainingSet$pure,], seed = 0xbadcafe, ...)
        RegPredictions = predict(RegressionModel, ValidationSet[!PureValPredictions,])
        Predictions[!PureValPredictions, Class] = RegPredictions$predictions
    }
    
    if (scale)
        Predictions = Predictions / rowSums(Predictions) * 100
    
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
PredictionResult = RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-allcovars-validation.csv", InflationAdjustment = 0)
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 16.3%, MAE 9.1%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 68%±4, kappa 0.60
cor(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100))^2 # 0.68

# Holdout with the ~100 uncorrelated covars
PredictionResult = RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-uncorrelated-validation.csv", InflationAdjustment = 0, covars=GetUncorrelatedPixelCovars())
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 16.4%, MAE 9.1%, pretty much no change but much better to explain
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 68%±4, kappa 0.59
cor(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100))^2 # 0.68
PlotHex(PredictionResult[,Classes], Truth[,Classes], "RF, single model, uncorrelated covariates")
PlotBox(PredictionResult[,Classes], Truth[,Classes], main="RF, single model, uncorrelated covariates")
OneToOneStatPlot(PredictionResult[,Classes], Truth[,Classes], "RF, single model, uncorrelated covariates")

PredictionResult = RFCV("../data/pixel-based/predictions/", "randomforest-twostep-truncated-allcovars-10folds.csv", InflationAdjustment = 1, TruncateZeroes = TRUE)
PredictionResult[rowSums(PredictionResult) == 0,] = rep(10,10) # Set cases of all 0 to all 10
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 17%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 71±2%, kappa 0.63 - this is much better
cor(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100))^2 # 0.60

# Holdout
PredictionResult = RFTrain("../data/pixel-based/predictions/", "randomforest-twostep-truncated-allcovars-validation.csv", InflationAdjustment = 1, TruncateZeroes = TRUE)
PredictionResult[rowSums(PredictionResult) == 0,] = rep(100/length(Classes),length(Classes)) # Set cases of all 0 to equal
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 18.3%, MAE 7.6%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 73±2%, kappa 0.66 - this is much better
cor(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100))^2 # 0.64

# Holdout and uncorrelated two-step
PredictionResult = RFTrain("../data/pixel-based/predictions/", "randomforest-twostep-truncated-uncorrelated-validation.csv", InflationAdjustment = 1, TruncateZeroes = TRUE, covars=GetUncorrelatedPixelCovars())
PredictionResult[rowSums(PredictionResult) == 0,] = rep(100/length(Classes),length(Classes)) # Set cases of all 0 to equal
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 18.3%, MAE 8.0%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 72±2%, kappa 0.63 - this is slightly better
cor(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100))^2 # 0.62
PlotHex(PredictionResult[,Classes], Truth[,Classes], "RF, two models, zeroes truncated, uncorrelated covariates")
PlotBox(PredictionResult[,Classes], Truth[,Classes], main="RF, two models, zeroes truncated, uncorrelated covariates")
OneToOneStatPlot(PredictionResult[,Classes], Truth[,Classes], "RF, two models, zeroes truncated, uncorrelated covariates")

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

# Three-model approach
PredictionResult = RFTrain3("../data/pixel-based/predictions/", "randomforest-threestep-allcovars-validation.csv")
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 18.1%, MAE 8.0%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 72±3%, kappa 0.65 - little difference from two-step model, slightly worse
cor(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100))^2 # 0.63

# Higher purity
PredictionResult = RFTrain3("../data/pixel-based/predictions/", "randomforest-threestep-p99-allcovars-validation.csv", purity_threshold=99)
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 17.3%, MAE 8.3%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 71±4%, kappa 0.64 - worse, closer to one step
cor(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100))^2 # 0.65

# Lower purity
PredictionResult = RFTrain3("../data/pixel-based/predictions/", "randomforest-threestep-p85-allcovars-validation.csv", purity_threshold=85)
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 19.3%, MAE 7.9%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 72±3%, kappa 0.65 - very similar to 95% purity
cor(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100))^2 # 0.65

# Three-model approach with uncorrelated covars
PredictionResult = RFTrain3("../data/pixel-based/predictions/", "randomforest-threestep-uncorrelated-validation.csv",  covars=GetUncorrelatedPixelCovars())
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 18.2%, MAE 8.5%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 70±4%, kappa 0.61
cor(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100))^2 # 0.61
PlotHex(PredictionResult[,Classes], Truth[,Classes], "RF, three models, uncorrelated covariates")
PlotBox(PredictionResult[,Classes], Truth[,Classes], main="RF, three models, uncorrelated covariates")
OneToOneStatPlot(PredictionResult[,Classes], Truth[,Classes], "RF, three models, uncorrelated covariates")

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
ggplot(data.frame(Prediction=unlist(PredictionResult), Truth=unlist(Truth)), aes(Truth, Prediction)) +
    geom_hex() + xlim(0, 100) + ylim(0, 100) +
    scale_fill_distiller(palette=7, trans="log") + #log scale
    geom_abline(slope=1, intercept=0) + ggtitle("Random Forest, Validation, three models")


TruthBins = unlist(Truth)
TruthBins = round(TruthBins, -1)
ValidationDF = data.frame(Truth=unlist(Truth), Bins=as.factor(TruthBins), Predicted=unlist(PredictionResult))
boxplot(Predicted~TruthBins, ValidationDF, xlab="Truth", ylab="Predicted")
OneToOne = data.frame(Predicted=seq(0, 100, 10), Bins=1:11)
lines(Predicted~Bins, OneToOne)

# Pure model
Covariates = GetAllPixelCovars()
FullFormula = paste0("~", paste(Covariates, collapse = "+"))
PureFormula = update.formula(FullFormula, paste0("as.factor(pure) ~ ."))
PureModel = ranger(PureFormula, Data.df)
PurityPredictions = predict(PureModel, Data.val)$prediction
PurityPredictions = ifelse(PurityPredictions == "TRUE", TRUE, FALSE)
AccuracyStats(PurityPredictions, Data.val$pure)
mean(PurityPredictions == Data.val$pure) # 81% accuracy, 85% if using 100% as a threshold; you can guess 50% of the times

PureData = Data.val[PurityPredictions,]
ClassFormula = update.formula(FullFormula, paste0("dominant.lc ~ ."))
ClassModel = ranger(ClassFormula, Data.val[PurityPredictions,])

