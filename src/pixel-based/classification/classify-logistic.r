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
Data.val = LoadValidationAndCovariates()
class(Data.val) = "data.frame"
Data.val = TidyData(Data.val)

AllCovars = GetAllPixelCovars()
UncorrelatedCovars = GetUncorrelatedPixelCovars()
Classes = GetCommonClassNames()

# Feature selection
FullModelFormula = formula(paste("dominant_lc~", paste0("scale(", AllCovars, ")", collapse="+")))
FullModel = multinom(FullModelFormula, Data.df, MaxNWts=2000)
ReducedModel = step(FullModel) # This takes forever as it rebuilds the model for each covariate

# Very little difference between all and uncorrelated covars
logmodel = multinom(paste("dominant_lc ~", paste0("scale(", AllCovars, ")", collapse="+")), Data.df, MaxNWts = 2000)
lm_uncor = multinom(paste("dominant_lc ~", paste0("scale(", UncorrelatedCovars, ")", collapse="+")), Data.df)
AIC(logmodel, lm_uncor) # the full model is better
DropCovars = AllCovars[!AllCovars %in% "aspect"]
lm_drop = multinom(paste("dominant_lc ~", paste0("scale(", DropCovars, ")", collapse="+")), Data.df, MaxNWts = 2000)
AIC(logmodel, lm_drop) # Smaller model is better
DropCovars = DropCovars[!DropCovars %in% "intercept"]
lm_drop2 = multinom(paste("dominant_lc ~", paste0("scale(", DropCovars, ")", collapse="+")), Data.df, MaxNWts = 2000)
AIC(lm_drop, lm_drop2) # Even dropping the colinear variables doesn't help, so the full model is the best

Predictions = predict(logmodel, Data.val[,GetAllPixelCovars()], type="probs")
Predictions = as.data.frame(Predictions)
summary(lm_drop)

#CVPrediction = CrossValidate(paste("dominant_lc ~", paste0("scale(", AllCovars, ")", collapse="+")), Data.df, multinom, predict, type="probs")

AccuracyStatisticsPlots(Predictions, Data.val[, colnames(Predictions)]/100) # RMSE of 18.7%, MAE of 9.8%
SCM(Predictions, as.matrix(Data.val[, colnames(Predictions)]/100), plot=TRUE, totals=TRUE, scale=TRUE) # 66%±4 accuracy, kappa 0.57
ggplot(data.frame(Prediction=unlist(Predictions), Truth=unlist(Data.val[, colnames(Predictions)]/100)), aes(Truth, Prediction)) + # Scatterplot: just as non-linear as RF
    geom_hex() +
    scale_fill_distiller(palette=7, trans="log") #log scale
cor(unlist(Predictions), unlist(Data.val[, colnames(Predictions)]/100))^2 # 0.59

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
AccuracyStatisticsPlots(matrix(1/length(Classes), nrow=nrow(Data.df), ncol=length(Classes)), Data.df[, colnames(Predictions)]/100) # RMSE of 30%
SCM(matrix(1/length(Classes), nrow=nrow(Data.df), ncol=length(Classes)), Data.df[, colnames(Predictions)]/100, plot=TRUE, totals=TRUE, scale=TRUE) #25%±4 accuracy, kappa 0.1; still surprisingly high, ought to be 10% and 0
