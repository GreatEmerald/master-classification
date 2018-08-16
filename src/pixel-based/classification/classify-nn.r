# Pixel-based classification using standard neural networks

library(neuralnet)
source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("pixel-based/utils/crossvalidation.r")
registerDoParallel(cores=10)
source("pixel-based/utils/subpixel-confusion-matrix.r") # Replace with package eventually
source("utils/accuracy-statistics.r")

Data.df = LoadTrainingAndCovariates()
class(Data.df) = "data.frame"
Data.df = AddZeroValueColumns(Data.df)
Data.df = TidyData(Data.df)

FullFormula = paste(paste(GetLargeClassNames(Data.df), collapse="+"), paste0(GetAllPixelCovars(), collapse="+"), sep="~")
HiddenNeurons = round(mean(c(length(GetLargeClassNames(Data.df)), length(GetAllPixelCovars()))))
NN_Predict = function(model, newdata, ...) # Wrapper for compute to conform to predict() format. The ... make sure R doesn't stop with "unused arguments".
{
    r = compute(model, newdata)
    return(r$net.result)
}
# Scale Data.df outside of the formula
Data.scaled = apply(Data.df[,GetAllPixelCovars()], 2, scale)
Data.scaled = cbind(Data.scaled, Data.df[,c(GetLargeClassNames(Data.df))])
# For some reason it needs to reach the threshold rather than stepmax to save the model weights!
CVResult = CrossValidate(FullFormula, Data.scaled, neuralnet, NN_Predict, fold_column=Data.df$dominant_lc, covariate_names=GetAllPixelCovars(), hidden=HiddenNeurons, stepmax=6000, threshold=6000, lifesign="full")
colnames(CVResult) = GetLargeClassNames(Data.df)

# Taken from the tile-based chain; TODO: put into utils!
ScaleNNPrediction = function(prediction, global=FALSE)
{
    PredMinScale = prediction
    Min = min(prediction)
    for (i in 1:nrow(prediction))
    {
        if (!global)
            Min = min(prediction[i,])
        PredMinScale[i,] = (prediction[i,]+abs(Min)) / sum(prediction[i,]+abs(Min))
    }
    #PredMinScale = data.frame(PredMinScale)
    #names(PredMinScale) = GetValidationNames()
    return(PredMinScale)
}

CVR = ScaleNNPrediction(CVResult)

AccuracyStatisticsPlots(CVR, Data.df[, colnames(CVR)]/100) # RMSE of 17%
SCM(CVR, as.matrix(Data.df[, colnames(CVR)]/100), plot=TRUE, totals=TRUE, scale=TRUE) # 55% accuracy, 0.45 kappa - that's worse than logistic
ggplot(data.frame(Prediction=c(CVR), Truth=unlist(Data.df[, colnames(CVR)]/100)), aes(Prediction, Truth)) + # Scatterplot:
    geom_hex() +
    scale_fill_distiller(palette=7, trans="log") #log scale

## What if we balance the dataset
CVResultBalanced = CrossValidate(FullFormula, Data.scaled, neuralnet, NN_Predict, fold_column=Data.df$dominant_lc, covariate_names=GetAllPixelCovars(), oversample=TRUE, hidden=HiddenNeurons, stepmax=6000, threshold=6000, lifesign="full")
