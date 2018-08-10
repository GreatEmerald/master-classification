# Util for cross-validation
library(caret)
source("utils/accuracy-statistics.r")

# Pass in the function that takes as input a data.frame and produces a cross-validated data.frame in return
CrossValidate = function(formula, data, train_function, predict_function, folds=10, fold_column="dominant_lc", ...)
{
    set.seed(0xfedbeef)
    folds = createFolds(data[,fold_column], folds)
    
    Predictions = NULL
    for (i in 1:length(folds))
    {
        Model = train_function(formula=formula, ..., data=data[-folds[[i]],])
        Prediction = predict_function(Model, ..., newdata=data[folds[[i]],])
        Predictions = rbind(Predictions, Prediction)
    }
    Predictions = Predictions[order(unlist(folds)),]
    return(Predictions)
}

# Validation metrics and plots
AccuracyStatisticsPlots = function(predicted, observed)
{
    AST = AccuracyStatTable(predicted, observed)
    print(AST)
    op = par(mfrow=c(2,2))
    barplot(AST$RMSE, names.arg=rownames(AST), main="RMSE")
    barplot(AST$MAE, names.arg=rownames(AST), main="MAE")
    barplot(AST$ME, names.arg=rownames(AST), main="ME")
    try(corrplot(cor(predicted, observed), method="ellipse"))
    par(op)
}
