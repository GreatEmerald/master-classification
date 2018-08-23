source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("pixel-based/utils/subpixel-confusion-matrix.r") # Replace with package eventually
source("utils/accuracy-statistics.r")
source("pixel-based/utils/crossvalidation.r")

Data.df = LoadTrainingAndCovariates()
class(Data.df) = "data.frame"
Data.df = AddZeroValueColumns(Data.df)
Data.df = TidyData(Data.df)

FullFormula = paste("dominant_lc", paste0(GetAllPixelCovars(), collapse="+"), sep="~")

library(GSIF)
test = spfkm(formula(FullFormula), Data.df["dominant_lc"], Data.df[,GetAllPixelCovars()])

AccuracyStatisticsPlots(test@mu, Data.df[, colnames(test@mu)]/100) # RMSE of 24%
SCM(test@mu, as.matrix(Data.df[, colnames(test@mu)]/100), plot=TRUE, totals=TRUE, scale=TRUE) # 49% accuracy, kappa 0.39: pretty poor

