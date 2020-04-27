# Multivariate RF?!
library(MultivariateRandomForest)
library(hydroGOF)
source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("pixel-based/utils/crossvalidation.r")
source("pixel-based/utils/RFTrain.r")
source("pixel-based/utils/subpixel-confusion-matrix.r")
source("utils/accuracy-statistics.r")

Data.df = LoadTrainingAndCovariates()
Data.df[is.na(Data.df)] = -9999
#Data.sp = Data.df
Data.df = st_set_geometry(Data.df, NULL)
Data.df = AddZeroValueColumns(Data.df)
Data.df = TidyData(Data.df, drop.cols=NULL)
#Data.sp = Data.sp[rownames(Data.df),]

# Validation data
Data.val = LoadValidationAndCovariates()
Data.val[is.na(Data.val)] = -9999
Val.sp = Data.val
Data.val = st_set_geometry(Data.val, NULL)
Data.val = TidyData(Data.val, drop.cols=NULL) # Drops around 1050
Val.sp = Val.sp[rownames(Data.val),]

AllClasses = GetCommonClassNames()
Covars = GetAllPixelCovars()

TrainMRF = function(ecozone)
{
  OutCSV = paste0("../data/pixel-based/predictions/multivarrf-", ecozone, ".csv")
  
  AllSamples = which(Data.df$bc_id == ecozone)
  # Limit number of samples to max 2500 due to exponential increase in time for calcualtion
  Samples = sample(AllSamples, min(length(AllSamples), 2500))
  ValSamples = Data.val$bc_id == ecozone
  
  if (!file.exists(OutCSV))
  {
    ClassStats = colSums(Data.df[Samples,AllClasses])
    Classes = AllClasses[ClassStats > 0] # Exclude all missing classes, don't need to predict those
    ToExclude = Classes[which.min(ClassStats[Classes])]
    InputClasses = Classes[!Classes %in% ToExclude] # Exclude the smallest remaining class, that will be the reference class
    RemainingClasses = InputClasses
    
    while(length(RemainingClasses) > 0){
    mrfPrediction = try(build_forest_predict(as.matrix(Data.df[Samples,Covars]), as.matrix(Data.df[Samples,RemainingClasses]),
        24,#trees
        4, #variables per split
        10,#variables at terminal node
        as.matrix(Data.val[ValSamples,Covars])))
    if (class(mrfPrediction) != "try-error")
      break
    # It's a random forest, so if it selects a random slice that is singular, it dies.
    # Let's try to work around that by dropping the most sparse classes.
    ToExclude = RemainingClasses[which.min(ClassStats[RemainingClasses])]
    RemainingClasses = RemainingClasses[!RemainingClasses %in% ToExclude]
    }
    
    # Recreate the missing columns
    mrfDF = as.data.frame(mrfPrediction)
    names(mrfDF) = RemainingClasses
    mrfDF[[ToExclude]] = 100-rowSums(mrfDF)
    for (ZeroCol in AllClasses[!AllClasses %in% c(RemainingClasses, ToExclude)])
      mrfDF[[ZeroCol]] = 0 # Readd excluded classes
    # Sort columns back to normal
    mrfDF = mrfDF[,AllClasses]
    write.csv(mrfDF, OutCSV, row.names=FALSE)
  } else mrfDF = read.csv(OutCSV)
  cbind(mrfDF, order=which(ValSamples))
}

Prediction = pblapply(unique(Data.df$bc_id), TrainMRF, cl=12)

ResultOrder = do.call(rbind, Prediction)
stopifnot(all(!duplicated(ResultOrder[,"order"]))) # No point should be in two zones
ResultInOrder = ResultOrder[order(ResultOrder[,"order"]),]
ResultInOrder = ResultInOrder[,!colnames(ResultInOrder) %in% "order"] # Remove order column
write.csv(ResultInOrder, "../data/pixel-based/predictions/multivarrf-all.csv", row.names=FALSE)

#Truth = Data.val[ValSamples,Classes]
#AccuracyStatisticsPlots(mrfDF[,Classes]/100, Truth[,Classes]/100) # RMSE 16%
#SCM(PredictionResult[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 66%, kappa 0.57
#cor(unlist(PredictionResult[,Classes]/100), unlist(Truth[,Classes]/100))^2 # 0.65
