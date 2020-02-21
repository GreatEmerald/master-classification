# MESMA (multiple-endmember spectral unmixing) module
# At the moment, there is no advertised MESMA function for points.
# However, the RStoolbox::mesma() function is a wrapper for one:
# RStoolbox::nnls_solver(x=predict_matrix, A=endmember_matrix, iterate=400, tolerance=0.000001)
library(RStoolbox)
library(hydroGOF)

source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("pixel-based/utils/crossvalidation.r")
source("pixel-based/utils/subpixel-confusion-matrix.r")
source("utils/accuracy-statistics.r")

Data.df = LoadTrainingAndCovariates()
#Data.df[is.na(Data.df)] = -9999
Data.df = st_set_geometry(Data.df, NULL)
#Data.df = AddZeroValueColumns(Data.df)
Data.df = TidyData(Data.df, drop.cols=NULL)

# Validation data
Data.val = LoadValidationAndCovariates()
Data.val[is.na(Data.val)] = -9999
#Val.sp = Data.val
Data.val = st_set_geometry(Data.val, NULL)
Data.val = TidyData(Data.val, drop.cols=NULL) # Drops around 1050
#Val.sp = Val.sp[rownames(Data.val),]

Classes = GetCommonClassNames()
Truth = Data.val[,Classes]

# Get pure points in training
PureThreshold = 100
PureData = Data.df[,Classes]>=PureThreshold
PureRows = apply(PureData, 1, any)
PureData = Data.df[PureRows, GetAllPixelCovars()]
#rownames(PureData) = Data.df[PureRows, "dominant_lc"]
EM = stats::aggregate(PureData, list(Data.df[PureRows, "dominant_lc"]), mean, na.rm=TRUE)
rownames(EM) = EM[,1]

MESMA = RStoolbox:::nnls_solver(as.matrix(Data.val[,GetAllPixelCovars()]), as.matrix(EM[,-1]), iterate=400, tolerance=0.000001)
colnames(MESMA) = c(rownames(EM), "RMSE")
# The result is unscaled!
MESMA = ScalePredictions(MESMA[,Classes], FALSE)

corval = cor(MESMA[,Classes], Truth[,Classes], use="pairwise.complete.obs")

corrplot::corrplot(corval, method="ellipse")
AccuracyStatisticsPlots(MESMA/100, Truth[,Classes]/100) # RMSE 46.1%, MAE 25.7%, significantly worse than intercept?!
SCM(MESMA/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 10±0.3%, kappa 0.03±0.004
NSE(unlist(MESMA/100), unlist(Truth[,Classes]/100)) # -1.37, worse than intercept by far

## What if we only use spectral info
MESMAS = RStoolbox:::nnls_solver(as.matrix(Data.val[,GetAllPixelCovars(TRUE)$spectral]), as.matrix(EM[,GetAllPixelCovars(TRUE)$spectral]), iterate=400, tolerance=0.000001)
colnames(MESMAS) = c(rownames(EM), "RMSE")
# The result is unscaled!
MESMAS = ScalePredictions(MESMAS[,Classes], FALSE)
AccuracyStatisticsPlots(MESMAS/100, Truth[,Classes]/100) # RMSE 34.4%, MAE 19.0%, similar to intercept but at least trees, crops and water are fine now
SCM(MESMAS/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 34±3%, kappa 0.22±0.04
NSE(unlist(MESMAS/100), unlist(Truth[,Classes]/100)) # -0.32, still a bit worse than intercept

# What if only yearly means
Covars = c("NDMI.year.median", "NIRv.year.median")
MESMAS = RStoolbox:::nnls_solver(as.matrix(Data.val[,Covars]), as.matrix(EM[,Covars]), iterate=400, tolerance=0.000001)
colnames(MESMAS) = c(rownames(EM), "RMSE")
# The result is unscaled!
MESMAS = ScalePredictions(MESMAS[,Classes], FALSE)
AccuracyStatisticsPlots(MESMAS/100, Truth[,Classes]/100) # RMSE 44.8%, MAE 25.1%, similar to intercept but at least trees, crops and water are fine now
SCM(MESMAS/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE, scale=TRUE) # OA 12±1%, kappa -0.00±0.02
NSE(unlist(MESMAS/100), unlist(Truth[,Classes]/100)) # -1.24
