# Predict pixels using Random Forest

library(ranger)
library(hydroGOF)
library(histmatch) # devtools::install_github("krlmlr/histmatch")
#library(caret)
source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("pixel-based/utils/crossvalidation.r")
source("pixel-based/utils/subpixel-confusion-matrix.r")
source("utils/accuracy-statistics.r")

Data.df = LoadTrainingAndCovariates()
Data.df[is.na(Data.df)] = -9999
#Data.sp = Data.df
Data.df = as.data.frame(Data.df)
Data.df = AddZeroValueColumns(Data.df)
Data.df = TidyData(Data.df)
#Data.sp = Data.sp[rownames(Data.df),]

# Make sure there are no NAs left
apply(Data.df[,GetAllPixelCovars()], 2, function(x){sum(is.na(x))}) / nrow(Data.df) * 100

# Validation data
Data.val = LoadValidationAndCovariates()
Data.val[is.na(Data.val)] = -9999
Val.sp = Data.val
class(Data.val) = "data.frame"
Data.val = TidyData(Data.val) # Drops around 450 # Now drops ~800!
Val.sp = Val.sp[rownames(Data.val),]

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
RFTrain = function(outdir, filename, InflationAdjustment=1, TruncateZeroes = FALSE, scale=TRUE, covars=GetAllPixelCovars(), overwrite=FALSE, ...)
{
    OutputFile = file.path(outdir, paste0("predictions-", filename))
    if (!overwrite && file.exists(OutputFile))
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
        
        # Dynamic feature selection: drop any covariates that have any NA values for this particular class
        RelevantRows = TrainingSet[[Class]] > 0 # Could also just look at dominant
        RemainingCovars = !apply(TrainingSet[RelevantRows, Covariates], 2, function(x){any(!is.finite(x))})
        RemainingNames = names(RemainingCovars)[RemainingCovars]
        print(paste("Covars with no NA values:", toString(RemainingNames)))
        # TODO: finish implementing
            
        #Formula = update.formula(FullFormula, paste0(Class, " ~ ."))
        Formula =  formula(paste0(Class, "~", paste(RemainingNames, collapse = "+")))
        
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
    
    write.csv(Predictions, OutputFile, row.names=FALSE)
    
    if (scale)
    {
        Predictions = Predictions / rowSums(Predictions) * 100
        # There is a possibility that all classes have been predicted as 0, so we can't normalise.
        # In that case we just keep them as 0%. It won't add up to 100%. Alternatively we can set it to 1/nclass.
        Predictions[is.nan(Predictions)] = 0
    }
    
    return(as.data.frame(Predictions))
}

# Three-step classification. More complex and doesn't share much code with the other approach, hence separate function.
RFTrain3 = function(outdir, filename, scale=TRUE, purity_threshold=95, covars=GetAllPixelCovars(), overwrite=FALSE, ...)
{
    OutputFile = file.path(outdir, paste0("predictions-threestep-", filename))
    if (!overwrite && file.exists(OutputFile))
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
    
    write.csv(Predictions, OutputFile, row.names=FALSE)
    
    if (scale)
        Predictions = Predictions / rowSums(Predictions) * 100
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
PredictionResult = PredictionUnscaled / rowSums(PredictionUnscaled) * 100
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 16.4%, MAE 8.9%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 69%±4, kappa 0.61±0.06
NSE(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.68

# Holdout with the ~100 uncorrelated covars
PredictionUnscaled = RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-uncorrelated-validation.csv", InflationAdjustment = 0, covars=GetUncorrelatedPixelCovars(), scale=FALSE)
PredictionResult = PredictionUnscaled / rowSums(PredictionUnscaled) * 100
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 16.6%, MAE 9.2%, pretty much no change but much better to explain
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 68%±4, kappa 0.60
NSE(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.67
NSE(PredictionResult[,Classes]/100, Truth[,Classes]/100) # All decent
PlotHex(PredictionResult[,Classes], Truth[,Classes], "RF, single model, uncorrelated covariates")
PlotBox(PredictionResult[,Classes], Truth[,Classes], main="RF, single model, uncorrelated covariates")
# Temp: more plot types to test
ggplot(data.frame(Prediction=unlist(PredictionResult[,Classes]), Truth=unlist(Truth[,Classes])), aes(Truth, Prediction)) +
        stat_density_2d(geom = "point", aes(size = stat(sqrt(density))), n = 50, contour = FALSE) + scale_size_area(max_size=10) +
        geom_abline(slope=1, intercept=0)
ggplot(data.frame(Prediction=unlist(PredictionResult[,Classes]), Truth=unlist(Truth[,Classes])), aes(Truth, Prediction)) +
        stat_density_2d(geom = "raster", aes(fill = stat(sqrt(density))), n=50, contour = FALSE) + scale_fill_distiller(palette="Spectral") +
        geom_abline(slope=1, intercept=0)
ggplot(data.frame(Prediction=unlist(PredictionResult[,Classes]), Truth=unlist(Truth[,Classes])), aes(Truth, Prediction)) +
        geom_point(alpha=0.01, size=2) + 
        geom_abline(slope=1, intercept=0)

OneToOneStatPlot(PredictionResult[,Classes], Truth[,Classes], "RF, single model, uncorrelated covariates")
ResidualBubblePlot(PredictionResult[,Classes], Truth[,Classes], Val.sp[["geometry"]])
## Make a raster image
# Points
plot(Val.sp["shrub"])
PR.sp = st_set_geometry(PredictionResult, Val.sp[["geometry"]])
plot(PR.sp["shrub"])
library(raster)
rast <- raster()
extent(rast) <- extent(PR.sp)
ncol(rast) <- 100
nrow(rast) <- 100
PR.ras = rasterize(PR.sp[Classes], rast, fun=max)
plot(PR.ras)
Val.ras = rasterize(Val.sp[Classes], rast, fun=max)
plot(Val.ras)

plotRGB(Val.ras, "shrub", "tree", "grassland", stretch="lin")
plotRGB(PR.ras, "shrub", "tree", "grassland", stretch="lin")
writeRaster(PR.ras, "../rf-2m-raster.tif")
writeRaster(Val.ras, "../validation-raster.tif")

# Histogram matching
HMPredictions = HistMatchPredictions(PredictionUnscaled[,Classes], Data.df[,Classes])
AccuracyStatisticsPlots(HMPredictions/100, Truth[,Classes]/100) # RMSE 22.0%, MAE 9.7%, awful, but ME is collapsed
SCM(HMPredictions/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 66%±2, kappa 0.58±0.03
NSE(unlist(HMPredictions/100), unlist(Truth[,Classes]/100)) # 0.43
NSE(HMPredictions/100, Truth[,Classes]/100) # All decent but quite a bit lower in comparison
PlotHex(HMPredictions, Truth[,Classes], "RF, single model, uncorrelated covariates, histogram matched")
PlotBox(HMPredictions, Truth[,Classes], main="RF, single model, uncorrelated covariates, histogram matched")
OneToOneStatPlot(HMPredictions, Truth[,Classes], "RF, single model, uncorrelated covariates, histogram matched") # 0 and 100 accuracy skyrockets, but the middle plummets

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
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 18.8%, MAE 8.0%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 72±2%, kappa 0.65±0.03 - this is slightly better
NSE(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.59
NSE(PredictionResult[,Classes]/100, Truth[,Classes]/100) # shrubs are negative!
PlotHex(PredictionResult[,"shrub"], Truth[,"shrub"], "RF shrubs, two models, zeroes truncated, uncorrelated covariates")
PlotHex(PredictionResult[,"tree"], Truth[,"tree"], "RF trees, two models, zeroes truncated, uncorrelated covariates")
PlotHex(PredictionResult[,Classes], Truth[,Classes], "RF, two models, zeroes truncated, uncorrelated covariates")
PlotBox(PredictionResult[,Classes], Truth[,Classes], main="RF, two models, zeroes truncated, uncorrelated covariates")
OneToOneStatPlot(PredictionResult[,Classes], Truth[,Classes], "RF, two models, zeroes truncated, uncorrelated covariates")
HMPredictions = HistMatchPredictions(PredictionResult[,Classes], Data.df[,Classes])
AccuracyStatisticsPlots(HMPredictions/100, Truth[,Classes]/100) # RMSE 18.8%, MAE 8.0%
SCM(HMPredictions/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 72±2%, kappa 0.63 - no change
NSE(unlist(HMPredictions/100), unlist(Truth[,Classes]/100)) # 0.56
NSE(HMPredictions/100, Truth[,Classes]/100) # Very bad ones got better, but good ones got worse
PlotHex(HMPredictions, Truth[,Classes], "RF, two models, zeroes truncated, uncorrelated covariates, histogram matched")
PlotBox(HMPredictions, Truth[,Classes], main="RF, two models, zeroes truncated, uncorrelated covariates, histogram matched")
OneToOneStatPlot(HMPredictions, Truth[,Classes], "RF, two models, zeroes truncated, uncorrelated covariates, histogram matched") # Worse across the board
svg("../rf-2m-mae.svg", width=9, height=3)
barplot(as.matrix(AccuracyStatTable(PredictionResult[,Classes], Truth[,Classes]))[,"MAE"], main="Mean Absolute Error")
dev.off()
svg("../rf-2m-me.svg", width=9, height=3)
barplot(as.matrix(AccuracyStatTable(PredictionResult[,Classes], Truth[,Classes]))[,"ME"], main="Mean Error (bias)")
dev.off()

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
PredictionUnscaled = RFTrain3("../data/pixel-based/predictions/", "randomforest-threestep-uncorrelated-unscaled-validation.csv", scale=FALSE, covars=GetUncorrelatedPixelCovars())
PredictionResult = PredictionUnscaled / rowSums(PredictionUnscaled) * 100
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 18.6%, MAE 8.2%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 71±4%, kappa 0.64
cor(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100))^2 # 0.61
NSE(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.59
NSE(PredictionResult[,Classes]/100, Truth[,Classes]/100)
PlotHex(PredictionResult[,Classes], Truth[,Classes], "RF, three models, uncorrelated covariates")
# ggsave("../2019-05-09-rf-3m-uncor-hex.pdf", width=5, height=4)
PlotBox(PredictionResult[,Classes], Truth[,Classes], main="RF, three models, uncorrelated covariates")
OneToOneStatPlot(PredictionResult[,Classes], Truth[,Classes], "RF, three models, uncorrelated covariates")
ResidualBubblePlot(PredictionResult[,Classes], Truth[,Classes], Val.sp[["geometry"]])
HMPredictions = HistMatchPredictions(PredictionUnscaled[,Classes], Data.df[,Classes])
AccuracyStatisticsPlots(HMPredictions/100, Truth[,Classes]/100) # RMSE 21.3%, MAE 9.1%
SCM(HMPredictions/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 68±3%, kappa 0.61
NSE(unlist(HMPredictions/100), unlist(Truth[,Classes]/100)) # 0.46
NSE(HMPredictions/100, Truth[,Classes]/100) # shrubs are awful
PlotHex(HMPredictions, Truth[,Classes], "RF, three models, uncorrelated covariates, histogram matched")
PlotBox(HMPredictions, Truth[,Classes], main="RF, three models, uncorrelated covariates, histogram matched")
OneToOneStatPlot(HMPredictions, Truth[,Classes], "RF, three models, uncorrelated covariates, histogram matched") # Helps zeroes, but really hurts the middle
# Excluding extremes
HMPredictions = HistMatchPredictions(PredictionUnscaled[,Classes], Data.df[,Classes], extremes=0)
AccuracyStatisticsPlots(HMPredictions/100, Truth[,Classes]/100) # RMSE 20.3%, MAE 9.9%
SCM(HMPredictions/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 66±8%, kappa 0.58
NSE(unlist(HMPredictions/100), unlist(Truth[,Classes]/100)) # 0.51
NSE(HMPredictions/100, Truth[,Classes]/100) # Not terrible now
PlotHex(HMPredictions, Truth[,Classes], "RF, three models, uncorrelated covariates, histogram matched except extremes (A)")
PlotBox(HMPredictions, Truth[,Classes], main="RF, three models, uncorrelated covariates, histogram matched except extremes (A)")
OneToOneStatPlot(HMPredictions, Truth[,Classes], "RF, three models, uncorrelated covariates, histogram matched except extremes (A)")
HMPredictions = HistMatchPredictions(PredictionUnscaled[,Classes], Data.df[,Classes], extremes=-1)
AccuracyStatisticsPlots(HMPredictions/100, Truth[,Classes]/100) # RMSE 20.6%, MAE 9.1%
SCM(HMPredictions/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 66±8%, kappa 0.60
NSE(unlist(HMPredictions/100), unlist(Truth[,Classes]/100)) # 0.50
NSE(HMPredictions/100, Truth[,Classes]/100) # Terrible again
PlotHex(HMPredictions, Truth[,Classes], "RF, three models, uncorrelated covariates, histogram matched except extremes (B)")
PlotBox(HMPredictions, Truth[,Classes], main="RF, three models, uncorrelated covariates, histogram matched except extremes (B)")
OneToOneStatPlot(HMPredictions, Truth[,Classes], "RF, three models, uncorrelated covariates, histogram matched except extremes (B)")


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

## Test all covars and fewer training vs fewer covars and more training
Data.df = LoadTrainingAndCovariates()
Data.df = as.data.frame(Data.df)
Data.df = AddZeroValueColumns(Data.df)
Data.df = TidyData(Data.df)
Data.val = LoadValidationAndCovariates()
class(Data.val) = "data.frame"
Data.val = TidyData(Data.val) # Drops around 450 # Now drops ~800!
Classes = GetCommonClassNames()
Truth = Data.val[,Classes]
PredictionResult = RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-uncorrelated-validation-nabyrow.csv", InflationAdjustment = 0, covars=GetUncorrelatedPixelCovars())
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 16.4%, MAE 9.1%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 68%±4, kappa 0.59
NSE(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.67
NSE(PredictionResult[,Classes]/100, Truth[,Classes]/100) # All decent
PlotHex(PredictionResult[,Classes], Truth[,Classes], "RF, single model, uncorrelated covariates, NA removed by row")
PlotBox(PredictionResult[,Classes], Truth[,Classes], main="RF, single model, uncorrelated covariates, NA removed by row")
OneToOneStatPlot(PredictionResult[,Classes], Truth[,Classes], "RF, single model, uncorrelated covariates, NA removed by row")




Data.df = LoadTrainingAndCovariates()
Data.df = as.data.frame(Data.df)
Data.df = AddZeroValueColumns(Data.df)
Data.df[is.na(Data.df)] = -1
Data.df = TidyData(Data.df) # 28000 vs 26000
Data.val = LoadValidationAndCovariates()
class(Data.val) = "data.frame"
Data.val[is.na(Data.val)] = -1
Data.val = TidyData(Data.val) # 3500 vs 2700
Classes = GetCommonClassNames()
Truth = Data.val[,Classes]
PredictionResult = RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-uncorrelated-validation-naminusone.csv", InflationAdjustment = 0, covars=GetUncorrelatedPixelCovars())
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 16.6%, MAE 9.2%, slight increase
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 68%±4, kappa 0.60
NSE(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.67
NSE(PredictionResult[,Classes]/100, Truth[,Classes]/100) # All decent, water is much better
PlotHex(PredictionResult[,Classes], Truth[,Classes], "RF, single model, uncorrelated covariates, NA set to -1")
PlotBox(PredictionResult[,Classes], Truth[,Classes], main="RF, single model, uncorrelated covariates, NA set to -1")
OneToOneStatPlot(PredictionResult[,Classes], Truth[,Classes], "RF, single model, uncorrelated covariates, NA set to -1")
# Water is much better, bare soil is much worse

# what if we set -9999?
Data.df = LoadTrainingAndCovariates()
Data.df = as.data.frame(Data.df)
Data.df = AddZeroValueColumns(Data.df)
Data.df[is.na(Data.df)] = -9999
Data.df = TidyData(Data.df) # 28000 vs 26000
Data.val = LoadValidationAndCovariates()
class(Data.val) = "data.frame"
Data.val[is.na(Data.val)] = -9999
Data.val = TidyData(Data.val) # 3500 vs 2700
Classes = GetCommonClassNames()
Truth = Data.val[,Classes]
PredictionResult = RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-uncorrelated-validation-naminusnine.csv", InflationAdjustment = 0, covars=GetUncorrelatedPixelCovars())
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 16.6%, MAE 9.1%, slightly better
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 68%±4, kappa 0.60
NSE(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.67
NSE(PredictionResult[,Classes]/100, Truth[,Classes]/100) # All decent, water is much better
PlotHex(PredictionResult[,Classes], Truth[,Classes], "RF, single model, uncorrelated covariates, NA set to -9999")
PlotBox(PredictionResult[,Classes], Truth[,Classes], main="RF, single model, uncorrelated covariates, NA set to -9999")
OneToOneStatPlot(PredictionResult[,Classes], Truth[,Classes], "RF, single model, uncorrelated covariates, NA set to -9999")
# No change

# And how about if we filter out the bare soil
Data.df = LoadTrainingAndCovariates()
Data.df = as.data.frame(Data.df)
Data.df = AddZeroValueColumns(Data.df)
Data.df = TidyData(Data.df, drop.cols=c("co", "slope")) # 28500 still
Data.df[is.na(Data.df)] = -9999
Data.df = TidyData(Data.df) # 28000
Data.val = LoadValidationAndCovariates()
class(Data.val) = "data.frame"
Data.val = TidyData(Data.val, drop.cols=c("co", "slope")) # 3500
Data.val[is.na(Data.val)] = -9999
Data.val = TidyData(Data.val) # 3500
Classes = GetCommonClassNames()
Truth = Data.val[,Classes]
PredictionResult = RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-uncorrelated-validation-naminusnine-noslope.csv", InflationAdjustment = 0, covars=GetUncorrelatedPixelCovars())
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 16.6%, MAE 9.2%, slightly worse again
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 68%±4, kappa 0.60
NSE(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.68
NSE(PredictionResult[,Classes]/100, Truth[,Classes]/100) # All decent, water is much better
PlotHex(PredictionResult[,Classes], Truth[,Classes], "RF, single model, uncorrelated covariates, NA set to -9999")
PlotBox(PredictionResult[,Classes], Truth[,Classes], main="RF, single model, uncorrelated covariates, NA set to -9999")
OneToOneStatPlot(PredictionResult[,Classes], Truth[,Classes], "RF, single model, uncorrelated covariates, NA set to -9999")
# No change
# So just filling with -9999 is fine

## Try to histmatch only the middle

Data.df = LoadTrainingAndCovariates()
Data.df = as.data.frame(Data.df)
Data.df = AddZeroValueColumns(Data.df)
Data.df[is.na(Data.df)] = -9999
Data.df = TidyData(Data.df) # 28000 vs 26000
Data.val = LoadValidationAndCovariates()
class(Data.val) = "data.frame"
Data.val[is.na(Data.val)] = -9999
Data.val = TidyData(Data.val) # 3500 vs 2700
Classes = GetCommonClassNames()
Truth = Data.val[,Classes]

# Test simply without double rescaling
PredictionResultUnscaled = RFTrain("../data/pixel-based/predictions/", "randomforest-onestep-uncorrelated-unscaled-validation.csv", InflationAdjustment = 0, covars=GetUncorrelatedPixelCovars(), scale=FALSE)
PredictionResult = PredictionResultUnscaled / rowSums(PredictionResultUnscaled) * 100
AccuracyStatisticsPlots(PredictionResult[,Classes]/100, Truth[,Classes]/100) # RMSE 16.6%, MAE 9.2%
SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 68%±4, kappa 0.60
NSE(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.67
NSE(PredictionResult[,Classes]/100, Truth[,Classes]/100) # All decent
PlotHex(PredictionResult[,Classes], Truth[,Classes], "RF, single model, uncorrelated covariates")
PlotBox(PredictionResult[,Classes], Truth[,Classes], main="RF, single model, uncorrelated covariates")
OneToOneStatPlot(PredictionResult[,Classes], Truth[,Classes], "RF, single model, uncorrelated covariates")
# Histogram matching
HMPredictions = HistMatchPredictions(PredictionResultUnscaled[,Classes], Data.df[,Classes])
AccuracyStatisticsPlots(HMPredictions/100, Truth[,Classes]/100) # RMSE 22.0%, MAE 9.7%, terrible
SCM(HMPredictions/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 66%±2, kappa 0.57, this is much worse
NSE(unlist(HMPredictions/100), unlist(Truth[,Classes]/100)) # 0.43
NSE(HMPredictions/100, Truth[,Classes]/100) # Awful, shrubs are off
PlotHex(HMPredictions, Truth[,Classes], "RF, single model, uncorrelated covariates, histogram matched")
PlotBox(HMPredictions, Truth[,Classes], main="RF, single model, uncorrelated covariates, histogram matched")
OneToOneStatPlot(HMPredictions, Truth[,Classes], "RF, single model, uncorrelated covariates, histogram matched") # 0 and 100 accuracy skyrockets, but the middle plummets

# And now if we try to match only the non-extremes
# Type 0: just ignore extreme values
HMPredictions = HistMatchPredictions(PredictionResultUnscaled[,Classes], Data.df[,Classes], extremes=0)
AccuracyStatisticsPlots(HMPredictions/100, Truth[,Classes]/100) # RMSE 22.0%, MAE 14.2%, the worst
SCM(HMPredictions/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 51%±7, kappa 0.42
NSE(unlist(HMPredictions/100), unlist(Truth[,Classes]/100)) # 0.43
NSE(HMPredictions/100, Truth[,Classes]/100) # Negatives
PlotHex(HMPredictions, Truth[,Classes], "RF, single model, uncorrelated covariates, histogram matched except extremes")
PlotBox(HMPredictions, Truth[,Classes], main="RF, single model, uncorrelated covariates, histogram matched except extremes")
OneToOneStatPlot(HMPredictions, Truth[,Classes], "RF, single model, uncorrelated covariates, histogram matched except extremes") # 0 and 100 accuracy skyrockets, but the middle plummets

# That was awful as expected, now to the quantile method (results should about equal the standard method)
HMPredictions = HistMatchPredictions(PredictionResultUnscaled[,Classes], Data.df[,Classes], extremes=-1)
AccuracyStatisticsPlots(HMPredictions/100, Truth[,Classes]/100) # RMSE 18.6%, MAE 9.9%, just a bit worse
SCM(HMPredictions/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 65%±5, kappa 0.57
NSE(unlist(HMPredictions/100), unlist(Truth[,Classes]/100)) # 0.59
NSE(HMPredictions/100, Truth[,Classes]/100) # All positive but all worse
PlotHex(HMPredictions, Truth[,Classes], "RF, single model, uncorrelated covariates, histogram matched except extremes, quantiles")
PlotBox(HMPredictions, Truth[,Classes], main="RF, single model, uncorrelated covariates, histogram matched except extremes, quantiles")
OneToOneStatPlot(HMPredictions, Truth[,Classes], "RF, single model, uncorrelated covariates, histogram matched except extremes, quantiles") # 0 and 100 accuracy skyrockets, but the middle plummets


