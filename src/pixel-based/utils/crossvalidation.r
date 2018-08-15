# Util for cross-validation
library(caret)
source("utils/accuracy-statistics.r")

# Pass in the function that takes as input a data.frame and produces a cross-validated data.frame in return
CrossValidate = function(formula, data, train_function, predict_function, folds=10, fold_column="dominant_lc", cv_seed=0xfedbeef, oversample=FALSE, ...)
{
    set.seed(cv_seed)
    folds = createFolds(data[,fold_column], folds)
    
    Predictions = NULL
    for (i in 1:length(folds))
    {
        TrainingData = data[-folds[[i]],]
        if (oversample)
            TrainingData = Oversample(TrainingData, seed=cv_seed)
        Model = train_function(formula=formula, ..., data=TrainingData)
        Prediction = predict_function(Model, ..., newdata=data[folds[[i]],])
        Predictions = rbind(Predictions, Prediction)
    }
    Predictions = Predictions[order(unlist(folds)),]
    return(Predictions)
}

# Validation metrics and plots
AccuracyStatisticsPlots = function(predicted, observed, ...)
{
    # RMSE values and correlation
    AST = AccuracyStatTable(predicted, observed)
    print(AST)
    op = par(mfrow=c(2,2))
    barplot(AST$RMSE, names.arg=rownames(AST), main="RMSE")
    barplot(AST$MAE, names.arg=rownames(AST), main="MAE")
    barplot(AST$ME, names.arg=rownames(AST), main="ME")
    try(corrplot::corrplot(cor(predicted, observed), method="ellipse"))
    par(op)
}

# Simple oversampling function
Oversample = function(Data, FactorName = "dominant_lc", seed=0xfedbeef)
{
    Factor = Data[[FactorName]]
    MaxSamples = max(table(Factor))
    Result=NULL
    
    set.seed(seed)
    for (ClassName in levels(Factor))
    {
        OneClassOnly = Data[Factor==ClassName,]
        ClassRows = sample(1:nrow(OneClassOnly), MaxSamples, replace=TRUE)
        ClassDF = OneClassOnly[ClassRows,]
        Result = rbind(Result, ClassDF)
    }
    return(Result)
}
