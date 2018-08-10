# Predict land cover fractions using a logistical regression.

library(nnet)
source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("pixel-based/utils/crossvalidation.r")
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
Predictions = predict(logmodel, Data.df, type="probs")
Predictions = Predictions * 100
summary(logmodel)

CVPrediction = CrossValidate(paste("dominant_lc ~", paste0("scale(", AllCovars, ")", collapse="+")), Data.df, multinom, predict, type="probs")

(AST = AccuracyStatTable(CVPrediction*100, Data.df[, colnames(CVPrediction)]))
barplot(AST$RMSE, names.arg=rownames(AST), main="RMSE")
barplot(AST$MAE, names.arg=rownames(AST), main="MAE")
barplot(AST$ME, names.arg=rownames(AST), main="ME")
corrplot(cor(CVPrediction, Data.df[, colnames(CVPrediction)]), method="ellipse")

AccuracyStatisticsPlots(CVPrediction*100, Data.df[, colnames(CVPrediction)])
AccuracyStatisticsPlots(Predictions, Data.df[, colnames(Predictions)])

(AST = AccuracyStatTable(Predictions, Data.df[, colnames(Predictions)]))
barplot(AST$RMSE, names.arg=rownames(AST), main="RMSE")
barplot(AST$MAE, names.arg=rownames(AST), main="MAE")
barplot(AST$ME, names.arg=rownames(AST), main="ME")
library(corrplot)
corrplot(cor(Predictions, Data.df[, colnames(Predictions)]), method="ellipse") # Pretty fascinting way to get a fuzzy confusion matrix

(InterceptAST = AccuracyStatTable(matrix(0, nrow=nrow(Data.df), ncol=length(Classes)), Data.df[, Classes]))
barplot(InterceptAST$RMSE, names.arg=rownames(InterceptAST), main="RMSE")
barplot(InterceptAST$ME, names.arg=rownames(InterceptAST), main="ME")

InterceptAST-AST

(InterceptAST2 = AccuracyStatTable(matrix(100/length(Classes), nrow=nrow(Data.df), ncol=length(Classes)), Data.df[, Classes]))
barplot(InterceptAST2$RMSE, names.arg=rownames(InterceptAST2), main="RMSE")
barplot(InterceptAST2$ME, names.arg=rownames(InterceptAST2), main="ME")

InterceptAST-InterceptAST2
