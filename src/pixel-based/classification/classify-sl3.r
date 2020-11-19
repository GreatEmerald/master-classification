# Ensemble learning using sl3
library(sl3)
library(hydroGOF)

library(future)
#plan("multicore", workers=3)

source("pixel-based/utils/load-sampling-data.r")
#source("pixel-based/utils/covariate-names.r")
source("pixel-based/utils/crossvalidation.r")
#source("pixel-based/utils/RFTrain.r")
source("pixel-based/utils/subpixel-confusion-matrix.r")
source("utils/accuracy-statistics.r")

Data.df = LoadTrainingAndCovariates()
Data.df[is.na(Data.df)] = -9999
Data.df = st_set_geometry(Data.df, NULL)
Data.df = AddZeroValueColumns(Data.df)
Data.df = TidyData(Data.df, drop.cols=NULL)

# Validation data
Data.val = LoadValidationAndCovariates()
Data.val[is.na(Data.val)] = -9999
Data.val = st_set_geometry(Data.val, NULL)
Data.val = TidyData(Data.val, drop.cols=NULL) # Drops around 1050

Classes = GetCommonClassNames()
Truth = Data.val[,Classes]

# Train a Cubist and RF, with RF as the ensembler

# Class = "water"
# task = make_sl3_Task(data = Data.df, covariates = GetAllPixelCovars(),
#                       outcome = Class, outcome_type="continuous")
# 
# RFModel = make_learner(Lrnr_ranger)
# CubistModel = make_learner(Lrnr_caret, "cubist")
# CombinedModel = make_learner(Stack, RFModel, CubistModel)
# MetaModel = make_learner(Lrnr_ranger)
# 
# FullModel = Lrnr_sl$new(learners = CombinedModel, metalearner = MetaModel)
# #FullModelTrained = FullModel$train(task)
# FullModelTrained = delayed_learner_train(FullModel, task)
# FullModelTrainedResult = FullModelTrained$compute()
# #Predicts = FullModelTrainedResult$predict()
# #head(Predicts)
# 
# ValTask = make_sl3_Task(data = Data.val, covariates = GetAllPixelCovars(),
#                         outcome = Class, outcome_type="continuous")
# Predicts = FullModelTrainedResult$predict(ValTask)
# cor(Predicts, Data.val$water)
# AccuracyStats(Predicts, Data.val$water)

# Now use binary relevance and automate the output

# Train wrapper
TrainSL3 = function(ClassFormula, data, covariates=GetAllPixelCovars())
{
    Class = all.vars(update(ClassFormula, .~0)) 
    
    task = make_sl3_Task(data = data, covariates = covariates,
                         outcome = Class, outcome_type="continuous")
    
    RFModel = make_learner(Lrnr_ranger)
    CubistModel = make_learner(Lrnr_caret, "cubist")
    CombinedModel = make_learner(Stack, RFModel, CubistModel)
    MetaModel = make_learner(Lrnr_ranger)
    
    FullModel = Lrnr_sl$new(learners = CombinedModel, metalearner = MetaModel)
    FullModelTrained = delayed_learner_train(FullModel, task)
    FullModelTrainedResult = FullModelTrained$compute(nworkers=3) # verbose=TRUE breaks with an error?!
    
    return(FullModelTrainedResult)
}

# Predict wrapper
PredictSL3 = function(Model, newdata)
{
    ValTask = make_sl3_Task(data = newdata, covariates = Model$training_task$nodes$covariates,
                            outcome = Model$training_task$nodes$outcome, outcome_type="continuous")
    Predicts = Model$predict(ValTask)
    return(Predicts)
}

# Run
Predictions = BinaryRelevance(paste0("~", paste(GetAllPixelCovars(), collapse = "+")), Data.df, TrainSL3, Data.val, PredictSL3, filename="../data/pixel-based/predictions/sl3-rf-cubist-rf.csv")

AccuracyStatisticsPlots(Predictions[,Classes]/100, Truth[,Classes]/100) # RMSE 17.7, MAE 8.6, similar to RF single model, lower MAE but higher RMSE
AccuracyStatTable(Predictions[,Classes]/100, Truth[,Classes]/100, TRUE) # RRMSE 0.18, RMAE 0.09
SCM(Predictions[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 70%±3, kappa 0.61±0.04
NSE(unlist(Predictions[,Classes]/100), unlist(Truth[,Classes]/100)) # 0.65
cor(unlist(Predictions[,Classes]/100), unlist(Truth[,Classes]/100))^2 # 0.65
PlotHex(Predictions[,Classes], Truth[,Classes], "RF+Cubist superlearner")
PlotBox(Predictions[,Classes], Truth[,Classes], main="RF+Cubist superlearner")
