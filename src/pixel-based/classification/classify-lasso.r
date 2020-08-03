# Classify using elastic net regression
library(glmnet)
library(doParallel)

source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("pixel-based/utils/crossvalidation.r")
registerDoParallel(cores=10)
source("pixel-based/utils/subpixel-confusion-matrix.r") # Replace with package eventually
source("utils/accuracy-statistics.r")


Data.df.orig = LoadTrainingAndCovariates()
Data.df.orig = st_set_geometry(Data.df.orig, NULL)
# Manually rescale
Data.df = Data.df.orig
#Data.df = RescaleBasedOn(Data.df.orig, Data.df.orig, GetAllPixelCovars())
#Data.df = NAToMean(Data.df, GetAllPixelCovars())
#Data.df[is.na(Data.df)] = -9999
Data.df[is.na(Data.df)] = 0
Data.df = TidyData(Data.df, drop.cols=NULL)

# Validation data
Data.val = LoadValidationAndCovariates()
Data.val = st_set_geometry(Data.val, NULL)
#Data.val = RescaleBasedOn(Data.val, Data.df.orig, GetAllPixelCovars())
#Data.val = NAToMean(Data.val, GetAllPixelCovars())
#Data.val[is.na(Data.val)] = -9999
Data.val[is.na(Data.val)] = 0
Data.val = TidyData(Data.val, drop.cols=NULL) # Drops around 1050

AllCovars = GetAllPixelCovars()
Classes = GetCommonClassNames()
Truth = Data.val[,Classes]

TargetMatrix = as.matrix(Data.df[,Classes])/100
CovarMatrix = as.matrix(Data.df[,AllCovars])

singlelasso = glmnet(CovarMatrix, TargetMatrix, family="mgaussian")
plot(singlelasso, label=TRUE, xvar="dev")
which(coef(singlelasso)$tree[,90] != 0) # Remaining covars
GetAllPixelCovars()[!(GetAllPixelCovars() %in% names(which(coef(singlelasso)$tree[,90] != 0)))] # Discarded covars
which(coef(singlelasso)$water[,50] != 0) # Remaining covars
GetAllPixelCovars()[!(GetAllPixelCovars() %in% names(which(coef(singlelasso)$water[,50] != 0)))] # Discarded covars
Predictions = predict(singlelasso, as.matrix(Data.val[,GetAllPixelCovars()]), type="response")
PredictionsClipped = Predictions
PredictionsClipped[PredictionsClipped < 0] = 0
PredictionsScaledA = apply(PredictionsClipped, 3, ScalePredictions, LeaveZeroes=FALSE)
PredictionsClipped[PredictionsClipped > 1] = 1
PredictionsScaledB = apply(PredictionsClipped, 3, ScalePredictions, LeaveZeroes=FALSE)
# There is almost no difference between A and B, A is just slightly better
AccChange = do.call(rbind, lapply(PredictionsScaledA, function(mat){AccuracyStats(mat[,Classes], Truth[,Classes])}))
plot(AccChange$RMSE, type="l")
plot(AccChange$MAE, type="l")
plot(AccChange$ME, type="l")
which.min(AccChange$RMSE) # 72
which.min(AccChange$MAE) # 90
# At 57 it seems nothing is dropped, coefficients are just shrunk
# What did we drop at 48?
for (i in 1:length(coef(singlelasso)))
    print(which(coef(singlelasso)[[i]][,48] == 0))
# jul.srad and mean.srad
# 90 is just OLS
# Let's try 72 (lambda=0.000401)
write.csv(PredictionsScaledA[[72]][,Classes], "../data/pixel-based/predictions/lasso-lambda0000401.csv", row.names=FALSE)
AccuracyStatisticsPlots(PredictionsScaledA[[72]][,Classes], Truth) # 21.4% RMSE, 12.6 MAE
SCM(PredictionsScaledA[[72]][,Classes]/100, Truth/100, plot=TRUE, totals=TRUE) # OA 56±0.04 kappa 0.43±0.05
NSE(unlist(PredictionsScaledA[[72]][,Classes])/100, unlist(Truth[,Classes]/100)) # 0.49

lambdas = cv.glmnet(CovarMatrix, TargetMatrix, family="mgaussian", parallel=TRUE)
lambdas[["lambda.min"]]
bestlambda = lambdas[["lambda.1se"]]
coef(lambdas, s=bestlambda) # Dropped roughness, mean.ndvi and intercept
lasso_prob <- predict(lambdas, newx = as.matrix(Data.val[,GetAllPixelCovars()]), s=bestlambda,type="response")
Predictions = as.data.frame(lasso_prob[,,1])*100
write.csv(Predictions, "../data/pixel-based/predictions/lasso-1se-9999.csv")
Predictions[Predictions < 0] = 0 # Remove negative vals
Predictions[Predictions > 100] = 100# Cap ones over 100
Predictions = ScalePredictions(Predictions, FALSE)

AccuracyStatisticsPlots(Predictions, Truth) # 22.3% RMSE, 13.4 MAE
SCM(Predictions/100, Truth/100, plot=TRUE, totals=TRUE) # OA 53±0.04 kappa 0.40±0.05
NSE(unlist(Predictions)/100, unlist(Truth[,Classes]/100)) # 0.45
PlotHex(Predictions, Truth[,Classes], "Lasso regression, lambda at 1se")
PlotBox(Predictions, Truth[,Classes], main="Lasso regression, lambda at 1se", binpredicted=TRUE)

# What if we use min (82 covars vs 71)
lasso_prob <- predict(lambdas, newx = as.matrix(Data.val[,GetAllPixelCovars()]), s=lambdas[["lambda.min"]],type="response")
Predictions = as.data.frame(lasso_prob[,,1])*100
write.csv(Predictions, "../data/pixel-based/predictions/lasso-min-9999.csv")
Predictions[Predictions < 0] = 0 # Remove negative vals
Predictions[Predictions > 100] = 100# Cap ones over 100
Predictions = ScalePredictions(Predictions, FALSE)

AccuracyStatisticsPlots(Predictions, Truth) # 22.1% RMSE, 13.3 MAE, tiny amount better
SCM(Predictions/100, Truth/100, plot=TRUE, totals=TRUE) # OA 54±0.04 kappa 0.40±0.05
NSE(unlist(Predictions)/100, unlist(Truth[,Classes]/100)) # 0.45
PlotHex(Predictions, Truth[,Classes], "Lasso regression, lambda at min")
PlotBox(Predictions, Truth[,Classes], main="Lasso regression, lambda at min", binpredicted=TRUE)

# If we scale the data

CovarMatrix = as.matrix(apply(Data.df[,GetAllPixelCovars()], 2, scale))

lambdas = cv.glmnet(CovarMatrix, TargetMatrix, family="mgaussian", parallel=TRUE)
lambdas[["lambda.min"]]
bestlambda = lambdas[["lambda.1se"]]
coef(lambdas, s=bestlambda) # Dropped roughness, mean.ndvi and intercept
lasso_prob <- predict(lambdas, newx = CovarMatrix, s=bestlambda,type="response")

AccuracyStatisticsPlots(as.data.frame(lasso_prob[,,1]), as.data.frame(TargetMatrix)) # 18.5% RMSE
