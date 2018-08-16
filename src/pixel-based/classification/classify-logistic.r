# Predict land cover fractions using a logistical regression.

library(nnet)
source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("pixel-based/utils/crossvalidation.r")
source("pixel-based/utils/subpixel-confusion-matrix.r") # Replace with package eventually
source("utils/accuracy-statistics.r")

Data.df = LoadTrainingAndCovariates()
class(Data.df) = "data.frame"
Data.df = AddZeroValueColumns(Data.df)
Data.df = TidyData(Data.df)

AllCovars = GetAllPixelCovars()
UncorrelatedCovars = GetUncorrelatedPixelCovars()
Classes = GetIIASAClassNames(TRUE)

# Very little difference between all and uncorrelated covars
logmodel = multinom(paste("dominant_lc ~", paste0("scale(", AllCovars, ")", collapse="+")), Data.df)
lm_uncor = multinom(paste("dominant_lc ~", paste0("scale(", UncorrelatedCovars, ")", collapse="+")), Data.df)
AIC(logmodel, lm_uncor) # the full model is better
DropCovars = AllCovars[!AllCovars %in% "aspect"]
lm_drop = multinom(paste("dominant_lc ~", paste0("scale(", DropCovars, ")", collapse="+")), Data.df)
AIC(logmodel, lm_drop) # the full model is still better
DropCovars = AllCovars[!AllCovars %in% "intercept"]
lm_drop = multinom(paste("dominant_lc ~", paste0("scale(", DropCovars, ")", collapse="+")), Data.df)
AIC(logmodel, lm_drop) # Even dropping the colinear variables doesn't help, so the full model is the best

Predictions = predict(logmodel, Data.df, type="probs")
summary(logmodel)

CVPrediction = CrossValidate(paste("dominant_lc ~", paste0("scale(", AllCovars, ")", collapse="+")), Data.df, multinom, predict, type="probs")

AccuracyStatisticsPlots(CVPrediction, Data.df[, colnames(CVPrediction)]/100) # RMSE of 17%
SCM(CVPrediction, as.matrix(Data.df[, colnames(CVPrediction)]/100), plot=TRUE, totals=TRUE, scale=TRUE) # 60% accuracy, kappa 0.51
ggplot(data.frame(Prediction=c(CVPrediction), Truth=unlist(Data.df[, colnames(CVPrediction)]/100)), aes(Prediction, Truth)) + # Scatterplot: just as non-linear as RF
    geom_hex() +
    scale_fill_distiller(palette=7, trans="log") #log scale

# How much does it overfit: when without CV
AccuracyStatisticsPlots(Predictions, Data.df[, colnames(Predictions)]/100) # RMSE of 17% still
SCM(Predictions, Data.df[, colnames(Predictions)]/100, plot=TRUE, totals=TRUE, scale=TRUE) #61% accuracy, kappa 0.51
# So CV has little effect

# What if we oversample to balance the dataset
CVPrediction = CrossValidate(paste("dominant_lc ~", paste0("scale(", AllCovars, ")", collapse="+")), Data.df, multinom, predict, type="probs", oversample=TRUE)
AccuracyStatisticsPlots(CVPrediction, Data.df[, colnames(CVPrediction)]/100) # RMSE of 17%
SCM(CVPrediction, as.matrix(Data.df[, colnames(CVPrediction)]/100), plot=TRUE, totals=TRUE, scale=TRUE) # 60% accuracy, kappa 0.51
# So there is no difference

# Modelling zeroes separately doesn't make sense in this case: the model sees labels, which are not zero. So only class balance is an issue.

# How does it compare to the intercept model
AccuracyStatisticsPlots(matrix(0.1, nrow=nrow(Data.df), ncol=length(Classes)), Data.df[, colnames(Predictions)]/100) # RMSE of 26%
SCM(matrix(0.1, nrow=nrow(Data.df), ncol=length(Classes)), Data.df[, colnames(Predictions)]/100, plot=TRUE, totals=TRUE, scale=TRUE) #19% accuracy, kappa 0.1; still surprisingly high, ought to be 10% and 0
