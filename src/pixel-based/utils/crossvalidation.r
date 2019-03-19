# Util for cross-validation
library(caret)
library(foreach)
library(doParallel)
library(ggplot2)
library(reshape2)
source("utils/accuracy-statistics.r")

# Pass in the function that takes as input a data.frame and produces a cross-validated data.frame in return
CrossValidate = function(formula, data, train_function, predict_function, folds=10, fold_column=data[,"dominant_lc"],
    covariate_names=names(data), cv_seed=0xfedbeef, oversample=FALSE, packages=NULL, ...)
{
    set.seed(cv_seed)
    folds = createFolds(fold_column, folds)
    
    Predictions = NULL
    #for (i in 1:length(folds))
    Predictions = foreach(fold=iter(folds), .combine=rbind, .multicombine=TRUE, .inorder=TRUE, .packages=packages) %dopar%
    {
        #fold = folds[[i]]
        TrainingData = data[-fold,]
        if (oversample)
            TrainingData = Oversample(TrainingData, fold_column=fold_column, seed=cv_seed)
        set.seed(cv_seed)
        Model = train_function(formula=formula, ..., data=TrainingData)
        #print(dim(Model[[1]]))
        #print(dim(Model[[2]]))
        Prediction = predict_function(Model, ..., newdata=data[fold,covariate_names])
        #print(dim(Prediction))
        #Predictions = rbind(Predictions, Prediction)
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
Oversample = function(Data, fold_column = Data[[FactorName]], seed=0xfedbeef)
{
    Factor = fold_column
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

# Plot a 1:1 hexplot, expects a data.frame rather than a matrix, and it should be 0-100 rather than 0-1
PlotHex = function(predicted, observed, main="")
{
    hp = ggplot(data.frame(Prediction=unlist(predicted), Truth=unlist(observed)), aes(Truth, Prediction)) +
        geom_hex() + xlim(0, 100) + ylim(0, 100) +
        scale_fill_distiller(palette="Spectral", trans="log") + #log scale, 7 was the oranges
        geom_abline(slope=1, intercept=0) + ggtitle(main)
    return(hp)
}

# Plot 1:1 boxplot
PlotBox = function(predicted, observed, ...)
{
    TruthBins = unlist(observed)
    TruthBins = round(TruthBins, -1)
    ValidationDF = data.frame(Truth=unlist(observed), Bins=as.factor(TruthBins), Predicted=unlist(predicted))
    boxplot(Predicted~TruthBins, ValidationDF, xlab="Truth", ylab="Predicted", ...)
    OneToOne = data.frame(Predicted=seq(0, 100, 10), Bins=1:11)
    lines(Predicted~Bins, OneToOne)
}

# Additional statistics per class: how well we predict 0, 100, 0<x<50, 50<x<100
OneToOneStats = function(predicted, observed, row.name="")
{
    predicted = unlist(predicted)
    observed = unlist(observed)
    
    ZeroPredictions = predicted[observed == 0] == 0
    HundredPredictions = predicted[observed == 100] == 100
    LRidx = observed > 0 & observed <= 50
    LRPredictions = predicted[LRidx] > 0 & predicted[LRidx] <= 50
    URidx = observed > 50 & observed < 100
    URPredictions = predicted[URidx] > 0 & predicted[URidx] <= 50
    
    ZeroAccuracy = mean(ZeroPredictions)
    ZeroSD = sd(ZeroPredictions)
    HundredAccuracy = mean(HundredPredictions)
    HundredSD = sd(HundredPredictions)
    LowerRange = mean(LRPredictions)
    LowerSD = sd(LRPredictions)
    UpperRange = mean(URPredictions)
    UpperSD = sd(URPredictions)
    
    Result = data.frame(ZeroAccuracy = ZeroAccuracy, ZeroSD=ZeroSD,
        LowerRange=LowerRange, LowerSD=LowerSD,
        UpperRange=UpperRange, UpperSD=UpperSD,
        HundredAccuracy=HundredAccuracy, HundredSD=HundredSD)
    rownames(Result) = row.name
    return(Result)
}

# Looped over all classes
OneToOneStatTable = function(predicted, observed, long=FALSE)
{
    Result = OneToOneStats(predicted, observed, "Overall")
    for (i in 1:ncol(observed))
    {
        Result = rbind(Result, OneToOneStats(predicted[,i], observed[,i], names(observed)[i]))
    }
    
    if (long) return(OneToOneStatTableToLong(Result))
    return(Result)
}

# Convert stat table to long format
OneToOneStatTableToLong = function(OTOST)
{
    LongAcc = OTOST[,seq(1, ncol(OTOST), 2)]
    LongSD = OTOST[,seq(2, ncol(OTOST), 2)]
    LongAcc$class = rownames(LongAcc)
    LongSD$class = rownames(LongSD)
    AccLong = melt(LongAcc, id.vars="class", variable.name="statistic", value.name="accuracy")
    SDLong = melt(LongSD, id.vars="class", variable.name="statistic", value.name="sd")
    Result = cbind(AccLong, sd=SDLong[,3])
    return(Result)
}

OneToOneStatPlot = function(predicted, observed, main="")
{
    OOTable = OneToOneStatTable(predicted, observed)
    OOTableLong = OneToOneStatTableToLong(OOTable)
    print(ggplot(OOTableLong, aes(class, accuracy, fill=statistic)) +
        geom_bar(stat="identity", position="dodge") +
        geom_errorbar(aes(ymax=accuracy+sd, ymin=accuracy-sd), position="dodge") +
        ggtitle(main))
    return(OOTable)
}
