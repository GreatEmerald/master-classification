# Classify using elastic net regression
library(glmnet)
library(doParallel)

source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("pixel-based/utils/crossvalidation.r")
registerDoParallel(cores=10)
source("pixel-based/utils/subpixel-confusion-matrix.r") # Replace with package eventually
source("utils/accuracy-statistics.r")

Data.df = LoadTrainingAndCovariates()
class(Data.df) = "data.frame"
#Data.df = AddZeroValueColumns(Data.df)
Data.df = TidyData(Data.df)

TargetMatrix = as.matrix(Data.df[,GetCommonClassNames()])/100
CovarMatrix = as.matrix(Data.df[,GetUncorrelatedPixelCovars()])

singlelasso = glmnet(CovarMatrix, TargetMatrix, family="mgaussian")
plot(singlelasso, label=TRUE, xvar="dev")
which(coef(singlelasso)$tree[,95] != 0)

lambdas = cv.glmnet(CovarMatrix, TargetMatrix, family="mgaussian", parallel=TRUE)
lambdas[["lambda.min"]]
bestlambda = lambdas[["lambda.1se"]]
coef(lambdas, s=bestlambda) # Dropped roughness, mean.ndvi and intercept
lasso_prob <- predict(lambdas, newx = CovarMatrix, s=bestlambda,type="response")

AccuracyStatisticsPlots(as.data.frame(lasso_prob[,,1]), as.data.frame(TargetMatrix)) # 18.5% RMSE
Truth = Data.val[,GetCommonClassNames()]
PredictionResult = as.data.frame(predict(lambdas, newx = as.matrix(Data.val[,GetAllPixelCovars()]), s=bestlambda,type="response")[,,1])*100


# If we scale the data

CovarMatrix = as.matrix(apply(Data.df[,GetAllPixelCovars()], 2, scale))

lambdas = cv.glmnet(CovarMatrix, TargetMatrix, family="mgaussian", parallel=TRUE)
lambdas[["lambda.min"]]
bestlambda = lambdas[["lambda.1se"]]
coef(lambdas, s=bestlambda) # Dropped roughness, mean.ndvi and intercept
lasso_prob <- predict(lambdas, newx = CovarMatrix, s=bestlambda,type="response")

AccuracyStatisticsPlots(as.data.frame(lasso_prob[,,1]), as.data.frame(TargetMatrix)) # 18.5% RMSE