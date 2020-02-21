# Classification using a cubist model
library(Cubist)
library(hydroGOF)

source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("pixel-based/utils/crossvalidation.r")
source("pixel-based/utils/subpixel-confusion-matrix.r")
source("utils/accuracy-statistics.r")

Data.df = LoadTrainingAndCovariates()
Data.df[is.na(Data.df)] = -9999
Data.df = st_set_geometry(Data.df, NULL)
Data.df = TidyData(Data.df, drop.cols=NULL)

# Validation data
Data.val = LoadValidationAndCovariates()
Data.val[is.na(Data.val)] = -9999
Data.val = st_set_geometry(Data.val, NULL)
Data.val = TidyData(Data.val, drop.cols=NULL) # Drops around 1050

Classes = GetCommonClassNames()
Truth = Data.val[,Classes]

# Formula interface, supports only a single formula
cubist.formula = function(formula, data, ...)
{
    MF = model.frame(formula, data=data)
    cubist(MF[,-1], MF[,1], ...)
}

Predictions = BinaryRelevance(paste0("~", paste(GetAllPixelCovars(), collapse = "+")), Data.df, cubist.formula, Data.val)
