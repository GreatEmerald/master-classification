# Util for cross-validation
library(caret)
library(foreach)
library(doParallel)
library(ggplot2)
library(reshape2)
library(scales)
library(pbapply)
source("utils/accuracy-statistics.r")
source("pixel-based/utils/load-sampling-data.r")

# Pass in the function that takes as input a data.frame and produces a cross-validated data.frame in return
CrossValidate = function(formula, data, train_function, predict_function, folds=10, fold_column=data[,"dominant_lc"],
    covariate_names=names(data), cv_seed=0xfedbeef, oversample=FALSE, packages=NULL, ...)
{
    set.seed(cv_seed)
    folds = createFolds(fold_column, folds)
    
    #Predictions = NULL
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

# Train on and predict over ecozone clusters.
ClusterTrain = function(formula, data, train_function, val_data, predict_function=predict, cv_seed=0xfedbeef, include_neighbours=FALSE, cluster_col="bc_id", ...)
{
    set.seed(cv_seed)
    
    TrainClusters = if (include_neighbours) ClusterNeighbours() else {
        UniqueECs = unique(data[[cluster_col]])
        UniqueECs = as.character(UniqueECs[!is.na(UniqueECs)])
        ECList = as.list(UniqueECs)
        names(ECList) = UniqueECs
        ECList
    }
    # Remove validation that does not belong to any cluster
    val_data = val_data[!is.na(val_data[[cluster_col]]),]
    
    Prediction = pblapply(TrainClusters, function(x) {
        model = train_function(formula=formula, ..., data=data[data[[cluster_col]] %in% x,]) # Train on zone plus (optionally) neighbours
        ClusterRows = val_data[[cluster_col]]==x[[1]]
        ResultMat = predict_function(model, ..., newdata=val_data[ClusterRows,]) # Predict on zone only
        if (!is.matrix(ResultMat))
            ResultMat = as.matrix(ResultMat)
        cbind(ResultMat, order=which(ClusterRows))
    })
    ResultOrder = do.call(rbind, Prediction)
    stopifnot(all(!duplicated(ResultOrder[,"order"]))) # No point should be in two zones
    ResultInOrder = ResultOrder[order(ResultOrder[,"order"]),]
    ResultInOrder = ResultInOrder[,!colnames(ResultInOrder) %in% "order"] # Remove order column
    
    return(ResultInOrder)
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
        geom_hex() +# xlim(0, 100) + ylim(0, 100) +
        scale_fill_gradient2(high="red", mid=muted("red"), low="grey90", midpoint=log(1000), trans="log") +
        #scale_fill_distiller(palette="Spectral", trans="log") + #log scale, 7 was the oranges
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

# Spatial residual bubbleplot
# Predicted and observed should be data.frames with 100 as max value
# none.threshold: What to take as "no bias"; ==0 is very rare, and 5% off is fine
ResidualBubblePlot = function(predicted, observed, geometry, none.threshold=5, main="")
{
    Resids = predicted-observed
    Resids.sf = st_set_geometry(Resids, geometry)
    Resids.long = reshape2::melt(Resids.sf, id.vars="geometry", variable.name="class")
    Resids.long$size = abs(Resids.long$value)
    Resids.long$type = ifelse(Resids.long$value > none.threshold, "positive", ifelse(Resids.long$value < -none.threshold, "negative", "none"))
    ggplot(Resids.long) + geom_sf(aes(colour=type, size=size), alpha=0.5) + 
        scale_colour_manual(values=c(positive="red", none="green", negative="blue")) + 
        scale_size(range=c(0.1, 1), breaks=c(0, 20, 40, 60, 80)) + 
        facet_wrap("class") + ggtitle(main)
}

# Perform histogram matching for each class
# extremes is about whether to match extremes; 1 is yes, 0 is not for predicted 0/100, -1 is not for the corresponding quantile
HistMatchPredictions = function(predicted, training=LoadTrainingAndCovariates(), extremes=1)
{
    HMPredictions = predicted
    for (Class in names(predicted))
    {
        if (extremes == 1) {
            HMPredictions[,Class] = histmatch(predicted[,Class], training[,Class])
        } else {
            if (extremes == 0) {
                ExtremeRowsP = predicted[,Class] == 0 | predicted[,Class] == 100
            } else if (extremes == -1) {
                PercentileT0 = mean(training[,Class] == 0)
                PercentileT100 = 1-mean(training[,Class] == 100)
                ExtremeRowsP = predicted[,Class] < quantile(predicted[,Class], PercentileT0) |
                               predicted[,Class] > quantile(predicted[,Class], PercentileT100)
            }
            ExtremeRowsT = training[,Class] == 0 | training[,Class] == 100
            HMPredictions[!ExtremeRowsP,Class] = histmatch(predicted[!ExtremeRowsP,Class], training[!ExtremeRowsT,Class])
        }
    }
        
    # Sometimes, the histograms match in the way that everything becomes 0, so it's impossible to scale everything.
    # In cases like that, restore original values.
    ZeroRows = apply(HMPredictions, 1, function(x)all(x==0))
    HMPredictions[ZeroRows,] = predicted[ZeroRows,]
    # Scale
    HMPredictions = HMPredictions / rowSums(HMPredictions) * 100
    
    return(HMPredictions)
}
