source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("utils/accuracy-statistics.r")

Data.df = LoadTrainingAndCovariates()
class(Data.df) = "data.frame"
Data.df = AddZeroValueColumns(Data.df)
Data.df = TidyData(Data.df)

FullFormula = paste("dominant_lc", paste0(GetAllPixelCovars(), collapse="+"), sep="~")

library(GSIF)
test = spfkm(formula(FullFormula), Data.df["dominant_lc"], Data.df[,GetAllPixelCovars()])

