## Random Forest training function

RFTrain = function(outdir, filename, InflationAdjustment=1, TruncateZeroes = FALSE, scale=TRUE,
    covars=GetAllPixelCovars(), overwrite=FALSE, PredictType="response", PredictQuantiles=0.5, ValidationSet = Data.val, ...)
{
    if (!dir.exists(outdir))
        dir.create(outdir)
    OutputFile = file.path(outdir, paste0("predictions-", filename))
    if (!overwrite && file.exists(OutputFile))
    {
        Predictions = read.csv(OutputFile)
        if (scale) Predictions = ScalePredictions(Predictions)
        return(Predictions)
    }
    quantreg = ifelse(PredictType=="response", FALSE, TRUE)
    
    Covariates = covars
    FullFormula = paste0("~", paste(Covariates, collapse = "+"))
    TrainingSet = Data.df
    #TrainingSet = Oversample(TrainingSet)
        
    Predictions = matrix(ncol=length(Classes), nrow=nrow(ValidationSet), dimnames=list(list(), Classes))
    for (Class in Classes)
    {
        print(Class)
        ZeroClass = paste("no", Class, sep=".")
        
        # Dynamic feature selection: drop any covariates that have any NA values for this particular class
        RelevantRows = TrainingSet[[Class]] > 0 # Could also just look at dominant
        # For some reason is.finite() in a loop fails...
        #RemainingCovars = !apply(TrainingSet[RelevantRows, Covariates], 2, function(x){any(!is.finite(x))})
        RemainingCovars = apply(TrainingSet[RelevantRows, Covariates], 2, function(x){all(!is.infinite(x) & !is.na(x) & !is.nan(x))})
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
                        NonzeroModel = ranger(Formula, TrainingSet[TrainingSet[,Class] > 0,], seed = 0xbadcafe, quantreg=quantreg, ...)
                    else if (InflationAdjustment == 2)
                        NonzeroModel = ranger(Formula, TrainingSet[TrainingSet[,Class] > 0 & TrainingSet[,Class] < 100,], seed = 0xbadcafe, quantreg=quantreg, ...)
                } else
                    NonzeroModel = ranger(Formula, TrainingSet, seed = 0xbadcafe, quantreg=quantreg, ...)
                ClassPredictions[NonZeroes] = predict(NonzeroModel, ValidationSet[NonZeroes,], type=PredictType, quantiles=PredictQuantiles)$prediction
            } else print("Everything was predicted to be zero!")
        } else {
            # Predict all
            rfmodel = ranger(Formula, TrainingSet, seed = 0xbadcafe, quantreg=quantreg, ...)
            ClassPredictions = predict(rfmodel, ValidationSet, type=PredictType, quantiles=PredictQuantiles)$prediction
        }
            
        Predictions[,Class] = ClassPredictions
    }
    
    write.csv(Predictions, OutputFile, row.names=FALSE)
    
    if (scale) Predictions = ScalePredictions(Predictions)
    
    return(as.data.frame(Predictions))
}


# Three-step classification. More complex and doesn't share much code with the other approach, hence separate function.
RFTrain3 = function(outdir, filename, scale=TRUE, purity_threshold=95, covars=GetAllPixelCovars(),
    overwrite=FALSE, PredictType="response", PredictQuantiles=0.5, ValidationSet = Data.val, ...)
{
    OutputFile = file.path(outdir, paste0("predictions-threestep-", filename))
    if (!overwrite && file.exists(OutputFile))
    {
        Predictions = read.csv(OutputFile)
        if (scale) Predictions = ScalePredictions(Predictions)
        return(Predictions)
    }
        
    Covariates = covars
    FullFormula = paste0("~", paste(Covariates, collapse = "+"))
    TrainingSet = Data.df
    #TrainingSet = Oversample(TrainingSet)
    TrainingSet$pure = apply(Data.df[,Classes], 1, max) > purity_threshold
    if (all(Classes %in% names(ValidationSet)))
        ValidationSet$pure = apply(ValidationSet[,Classes], 1, max) > purity_threshold
    
    Predictions = matrix(ncol=length(Classes), nrow=nrow(ValidationSet), dimnames=list(list(), Classes))
    
    # Step one: pure/non-pure binary classification
    PureModel = ranger(paste0("as.factor(pure)", FullFormula), TrainingSet, seed = 0xbadcafe, ...)
    PureValPredictions = predict(PureModel, ValidationSet)
    PureValPredictions = as.logical(PureValPredictions$predictions)
    if (all(Classes %in% names(ValidationSet)))
    {
        PurityAcc = mean(PureValPredictions == ValidationSet$pure)
        print(paste("Built purity classifier, accuracy:", PurityAcc))
    }
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
        
        RegressionModel = ranger(Formula, TrainingSet[!TrainingSet$pure,], seed = 0xbadcafe, quantreg=PredictType!="response", ...)
        RegPredictions = predict(RegressionModel, ValidationSet[!PureValPredictions,], type=PredictType, quantiles=PredictQuantiles)
        Predictions[!PureValPredictions, Class] = RegPredictions$predictions
    }
    
    write.csv(Predictions, OutputFile, row.names=FALSE)
    
    if (scale) Predictions = ScalePredictions(Predictions)
    
    return(as.data.frame(Predictions))
}
