# Predict land cover fractions using a logistical regression.

library(nnet)
library(hydroGOF)
library(future)
source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("pixel-based/utils/crossvalidation.r")
source("pixel-based/utils/subpixel-confusion-matrix.r") # Replace with package eventually
source("utils/accuracy-statistics.r")

Data.df = LoadTrainingAndCovariates()
Data.df = st_set_geometry(Data.df, NULL)
# Manually rescale
#Data.df = RescaleBasedOn(Data.df, Data.df, GetAllPixelCovars())
#Data.df = NAToMean(Data.df, GetAllPixelCovars())
#Data.df[is.na(Data.df)] = -9999
Data.df[is.na(Data.df)] = 0
Data.df = TidyData(Data.df, drop.cols=NULL)

# Validation data
Data.val = LoadValidationAndCovariates()
Data.val = st_set_geometry(Data.val, NULL)
#Data.val = RescaleBasedOn(Data.val, Data.df, GetAllPixelCovars())
#Data.val = NAToMean(Data.val, GetAllPixelCovars())
#Data.val[is.na(Data.val)] = -9999
Data.val[is.na(Data.val)] = 0
Data.val = TidyData(Data.val, drop.cols=NULL) # Drops around 1050

AllCovars = GetAllPixelCovars()
Classes = GetCommonClassNames()
Truth = Data.val[,Classes]

# Feature selection
#FullModelFormula = formula(paste("dominant_lc~", paste0("scale(", AllCovars, ")", collapse="+")))
FullModelFormula = formula(paste("dominant_lc~", paste(AllCovars, collapse="+")))
set.seed(0xfedbeef)
FullModel = multinom(FullModelFormula, Data.df, MaxNWts=2000)

Predictions = predict(FullModel, Data.val[,GetAllPixelCovars()], type="probs")
AccuracyStatisticsPlots(Predictions[,Classes], Truth[,Classes]/100) # RMSE 25.3, MAE 15.6, relatively poor # 21.7 12.2 formula-scaled -9999 # 21.9 12.4 unscaled 0
SCM(Predictions[,Classes], Truth[,Classes]/100, plot=TRUE, totals=TRUE)
NSE(unlist(as.data.frame(Predictions[,Classes])), unlist(Truth[,Classes]/100))
PlotHex(as.data.frame(Predictions[,Classes]*100), Truth[,Classes], "Logistic regression, all covariates")
PlotBox(as.data.frame(Predictions[,Classes]*100), Truth[,Classes], main="Logistic regression, all covariates", binpredicted=TRUE) # Very fuzzy, the top does not predict well at all

# Train longer
set.seed(0xfedbeef)
FullModel = multinom(FullModelFormula, Data.df, MaxNWts=2000, maxit=200)

Predictions = predict(FullModel, Data.val[,GetAllPixelCovars()], type="probs")
AccuracyStatisticsPlots(Predictions[,Classes], Truth[,Classes]/100) # RMSE 21.6, MAE 12.1, very slightly better
SCM(Predictions[,Classes], Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 58%±4, kappa 0.46±0.06
NSE(unlist(as.data.frame(Predictions[,Classes])), unlist(Truth[,Classes]/100)) # 0.48
PlotHex(as.data.frame(Predictions[,Classes]*100), Truth[,Classes], "Logistic regression, all covariates, 200 iterations")
PlotBox(as.data.frame(Predictions[,Classes]*100), Truth[,Classes], main="Logistic regression, all covariates, 200 iterations", binpredicted=TRUE)
write.csv(Predictions[,Classes]*100, "../data/pixel-based/predictions/logistic-na0.csv", row.names=FALSE)

ReducedModel = step(FullModel) # This takes forever as it rebuilds the model for each covariate

# Very little difference between all and uncorrelated covars
#logmodel = multinom(paste("dominant_lc ~", paste0("scale(", AllCovars, ")", collapse="+")), Data.df, MaxNWts = 2500)
#lm_uncor = multinom(paste("dominant_lc ~", paste0("scale(", UncorrelatedCovars, ")", collapse="+")), Data.df)
#AIC(logmodel, lm_uncor) # the uncor model is better
#DropCovars = UncorrelatedCovars[!UncorrelatedCovars %in% "aspect"]
#lm_drop = multinom(paste("dominant_lc ~", paste0("scale(", DropCovars, ")", collapse="+")), Data.df, MaxNWts = 2000)
#AIC(lm_uncor, lm_drop) # Smaller model is better
#DropCovars = DropCovars[!DropCovars %in% "intercept"]
#lm_drop2 = multinom(paste("dominant_lc ~", paste0("scale(", DropCovars, ")", collapse="+")), Data.df, MaxNWts = 2000)
#AIC(lm_drop, lm_drop2) # Even dropping the colinear variables doesn't help, so the full model is the best
# Try growing the model
SmallerModelTask = future({step(multinom(dominant_lc~1, Data.df), paste("dominant_lc ~", paste0(AllCovars, collapse="+")), direction="forward")}) # takes long but may be useful to run
resolved(SmallerModelTask)
SmallerModel = value(SmallerModelTask)
# Result: multinom(formula = dominant_lc ~ scale(mean.ndvi) + scale(BDTICM.M.BDTICM_M) + 
#     scale(min.srad) + scale(bio12) + scale(max.vapr) + scale(bio10) + 
#     scale(ndvi.iqr) + scale(wc2.0_30s_prec_03) + scale(wc2.0_30s_prec_08) + 
#     scale(bio17) + scale(bio8) + scale(bio2) + scale(wc2.0_30s_vapr_08) + 
#     scale(wc2.0_30s_wind_10) + scale(x), data = Data.df)
SmallerModelBIC = step(multinom(dominant_lc~1, Data.df), paste("dominant_lc ~", paste0("scale(", UncorrelatedCovars, ")", collapse="+")), direction="forward", k=log(nrow(Data.df)))

Predictions = predict(SmallerModel, Data.val[,GetAllPixelCovars()], type="probs")
Predictions = as.data.frame(Predictions)

#CVPrediction = CrossValidate(paste("dominant_lc ~", paste0("scale(", AllCovars, ")", collapse="+")), Data.df, multinom, predict, type="probs")

AccuracyStatisticsPlots(Predictions, Data.val[, colnames(Predictions)]/100) # RMSE of 19.7%, MAE of 10.7%
SCM(Predictions, as.matrix(Data.val[, colnames(Predictions)]/100), plot=TRUE, totals=TRUE, scale=TRUE) # 63%±5 accuracy, kappa 0.53
ggplot(data.frame(Prediction=unlist(Predictions), Truth=unlist(Data.val[, colnames(Predictions)]/100)), aes(Truth, Prediction)) + # Scatterplot: just as non-linear as RF
    geom_hex() +
    scale_fill_distiller(palette=7, trans="log") #log scale
cor(unlist(Predictions), unlist(Data.val[, colnames(Predictions)]/100))^2 # 0.59
NSE(unlist(Predictions), unlist(Data.val[, colnames(Predictions)]/100)) # 0.54

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
