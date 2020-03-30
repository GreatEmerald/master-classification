# Use Keras for a neural network instead of neuralnet

# Set environment variables
source("packrat/init.R")
# Interface with a local conda environment: this needs to be configured for each system
Sys.setenv("PATH" = paste0("/usr/local/cuda-10.2/bin:", Sys.getenv("HOME"), "/miniconda3/bin:", Sys.getenv("PATH")))
Sys.setenv(CUDA_HOME="/usr/local/cuda-10.2")
#Sys.setenv("LD_LIBRARY_PATH" = paste0(Sys.getenv("HOME"), "/miniconda3/envs/r-tensorflow/lib:", Sys.getenv("LD_LIBRARY_PATH")))
library(keras)
library(hydroGOF)

source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("pixel-based/utils/crossvalidation.r")
#registerDoParallel(cores=10)
source("pixel-based/utils/subpixel-confusion-matrix.r") # Replace with package eventually
source("utils/accuracy-statistics.r")

Data.df.orig = LoadTrainingAndCovariates()
Data.df.orig = st_set_geometry(Data.df.orig, NULL)
# Manually rescale
Data.df = Data.df.orig
#Data.df = RescaleBasedOn(Data.df.orig, Data.df.orig, GetAllPixelCovars())
#Data.df = NAToMean(Data.df, GetAllPixelCovars())
Data.df[is.na(Data.df)] = -9999
#Data.df[is.na(Data.df)] = 0
Data.df = TidyData(Data.df, drop.cols=NULL)

# Validation data
Data.val = LoadValidationAndCovariates()
Data.val = st_set_geometry(Data.val, NULL)
#Data.val = RescaleBasedOn(Data.val, Data.df.orig, GetAllPixelCovars())
#Data.val = NAToMean(Data.val, GetAllPixelCovars())
Data.val[is.na(Data.val)] = -9999
#Data.val[is.na(Data.val)] = 0
Data.val = TidyData(Data.val, drop.cols=NULL) # Drops around 1050

TargetMatrix = as.matrix(Data.df[,GetCommonClassNames()])/100
CovarMatrix = as.matrix(Data.df[,GetAllPixelCovars()])

model = keras_model_sequential() %>% 
  layer_dense(units = 128, activation = 'relu', input_shape = c(67)) %>% 
  #layer_dropout(rate = 0.5) %>% 
  layer_batch_normalization() %>%
  #layer_dense(units = 128, activation = 'relu') %>% 
  #layer_batch_normalization() %>%
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 64, activation = 'relu') %>% 
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 7, activation = 'sigmoid') %>% 
  compile(
    loss = 'mean_absolute_error',
    optimizer = optimizer_adam(lr = 0.0001, decay = 1e-5),
    #optimizer = optimizer_sgd(lr = 0.01, decay = 1e-4, momentum = 0.9, nesterov = TRUE),
    metrics = c('accuracy')     
  )


# # define train/val set
# 
# 
# # train
# for (i in (1:300)){
#   history = fit(model, CovarMatrix[], TargetMatrix[], epochs = 1, batch_size = 128)
#   plot(history)
#   
#   # save model state
# }


schedule = function(epoch, lr)
{
  # if(epoch==100) {
  #   print('reducing lr...')
  #   lr = lr / 10
  # }
  return(lr)
}


history = fit(model, CovarMatrix, TargetMatrix, epochs = 100, batch_size = 256, validation_split=0.3)
              # callbacks=list(callback_learning_rate_scheduler(schedule),callback_reduce_lr_on_plateau(monitor = "val_loss", factor = 0.1,patience = 10)))
plot(history)

# evaluate
#score <- model %>% evaluate(x_test, y_test, batch_size = 128)
#score

ValidMatrix = as.matrix(Data.val[,GetAllPixelCovars()])
TruthMatrix = as.matrix(Data.val[,GetCommonClassNames()])
preds = predict(model, ValidMatrix)
colnames(preds) = colnames(TargetMatrix)
round(head(preds), 2)
rowSums(preds)

round(head(TruthMatrix), 2)
round(head(preds), 2)

Predictions = preds
Predictions[Predictions < 0] = 0
Predictions[Predictions > 1] = 1
Predictions = ScalePredictions(as.data.frame(Predictions*100), FALSE)
AccuracyStatisticsPlots(Predictions, Data.val[,GetCommonClassNames()]) # 29.7% RMSE, 12.5 MAE
SCM(Predictions/100, Data.val[,GetCommonClassNames()]/100, plot=TRUE, totals=TRUE) # OA 56±0 kappa 0.39±0.01
NSE(unlist(Predictions)/100, unlist(Data.val[,GetCommonClassNames()]/100)) # 0.02
PlotHex(Predictions, Data.val[,GetCommonClassNames()], "Neural networks, 2 hidden layers (128&64), sigmoid activation, adam optimiser")
PlotBox(Predictions, Data.val[,GetCommonClassNames()], main="Neural networks, 2 hidden layers (128&64), sigmoid activation, adam optimiser", binpredicted=TRUE)

## Try softmax activation

model = keras_model_sequential() %>% 
  layer_dense(units = 128, activation = 'relu', input_shape = c(67)) %>% 
  #layer_dropout(rate = 0.5) %>% 
  layer_batch_normalization() %>%
  #layer_dense(units = 128, activation = 'relu') %>% 
  #layer_batch_normalization() %>%
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 64, activation = 'relu') %>% 
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 7, activation = 'softmax') %>% 
  compile(
    loss = 'mean_absolute_error',
    optimizer = optimizer_adam(lr = 0.0001, decay = 1e-5),
    #optimizer = optimizer_sgd(lr = 0.01, decay = 1e-4, momentum = 0.9, nesterov = TRUE),
    metrics = c('accuracy')     
  )
fit(model, CovarMatrix, TargetMatrix, epochs = 100, batch_size = 256, validation_split=0.3)

preds = predict(model, ValidMatrix)
colnames(preds) = colnames(TargetMatrix)
Predictions = as.data.frame(preds*100)
AccuracyStatisticsPlots(Predictions, Data.val[,GetCommonClassNames()]) # 25.1% RMSE, 10.6 MAE, much better
SCM(Predictions/100, Data.val[,GetCommonClassNames()]/100, plot=TRUE, totals=TRUE) # OA 63±1 kappa 0.51±0.02
NSE(unlist(Predictions)/100, unlist(Data.val[,GetCommonClassNames()]/100)) # 0.30
PlotHex(Predictions, Data.val[,GetCommonClassNames()], "Neural networks, 2 hidden layers (128&64), softmax activation, adam optimiser")
PlotBox(Predictions, Data.val[,GetCommonClassNames()], main="Neural networks, 2 hidden layers (128&64), softmax activation, adam optimiser", binpredicted=TRUE)


## Try with NA set to 0
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
Data.val = TidyData(Data.val, drop.cols=NULL)

model = keras_model_sequential() %>% 
  layer_dense(units = 128, activation = 'relu', input_shape = c(67)) %>% 
  #layer_dropout(rate = 0.5) %>% 
  layer_batch_normalization() %>%
  #layer_dense(units = 128, activation = 'relu') %>% 
  #layer_batch_normalization() %>%
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 64, activation = 'relu') %>% 
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 7, activation = 'softmax') %>% 
  compile(
    loss = 'mean_absolute_error',
    optimizer = optimizer_adam(lr = 0.0001, decay = 1e-5),
    #optimizer = optimizer_sgd(lr = 0.01, decay = 1e-4, momentum = 0.9, nesterov = TRUE),
    metrics = c('accuracy')     
  )
fit(model, CovarMatrix, TargetMatrix, epochs = 100, batch_size = 256, validation_split=0.3)

preds = predict(model, ValidMatrix)
colnames(preds) = colnames(TargetMatrix)
Predictions = as.data.frame(preds*100)
AccuracyStatisticsPlots(Predictions, Data.val[,GetCommonClassNames()]) # 24.8% RMSE, 10.3 MAE, a bit better
SCM(Predictions/100, Data.val[,GetCommonClassNames()]/100, plot=TRUE, totals=TRUE) # OA 64±1 kappa 0.52±0.02
NSE(unlist(Predictions)/100, unlist(Data.val[,GetCommonClassNames()]/100)) # 0.31
PlotHex(Predictions, Data.val[,GetCommonClassNames()], "Neural networks, 2 hidden layers (128&64), softmax activation, adam optimiser, NA to 0")
PlotBox(Predictions, Data.val[,GetCommonClassNames()], main="Neural networks, 2 hidden layers (128&64), softmax activation, adam optimiser, NA to 0", binpredicted=TRUE)

# Remove dropouts
model = keras_model_sequential() %>% 
  layer_dense(units = 128, activation = 'relu', input_shape = c(67)) %>% 
  #layer_dropout(rate = 0.5) %>% 
  layer_batch_normalization() %>%
  #layer_dense(units = 128, activation = 'relu') %>% 
  #layer_batch_normalization() %>%
  #layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 64, activation = 'relu') %>% 
  layer_batch_normalization() %>%
  #layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 7, activation = 'softmax') %>% 
  compile(
    loss = 'mean_absolute_error',
    optimizer = optimizer_adam(lr = 0.0001, decay = 1e-5),
    #optimizer = optimizer_sgd(lr = 0.01, decay = 1e-4, momentum = 0.9, nesterov = TRUE),
    metrics = c('accuracy')     
  )
fit(model, CovarMatrix, TargetMatrix, epochs = 100, batch_size = 256, validation_split=0.3)

preds = predict(model, ValidMatrix)
colnames(preds) = colnames(TargetMatrix)
Predictions = as.data.frame(preds*100)
AccuracyStatisticsPlots(Predictions, Data.val[,GetCommonClassNames()]) # 24.0% RMSE, 10.0 MAE, a bit better again
SCM(Predictions/100, Data.val[,GetCommonClassNames()]/100, plot=TRUE, totals=TRUE) # OA 65±1 kappa 0.54±0.02
NSE(unlist(Predictions)/100, unlist(Data.val[,GetCommonClassNames()]/100)) # 0.36
PlotHex(Predictions, Data.val[,GetCommonClassNames()], "Neural networks, 2 hidden layers (128&64), softmax, adam, NA to 0, no dropout")
PlotBox(Predictions, Data.val[,GetCommonClassNames()], main="Neural networks, 2 hidden layers (128&64), softmax, adam, NA to 0, no dropout", binpredicted=TRUE)

# try sgd
model = keras_model_sequential() %>% 
  layer_dense(units = 128, activation = 'relu', input_shape = c(67)) %>% 
  #layer_dropout(rate = 0.5) %>% 
  layer_batch_normalization() %>%
  #layer_dense(units = 128, activation = 'relu') %>% 
  #layer_batch_normalization() %>%
  #layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 64, activation = 'relu') %>% 
  layer_batch_normalization() %>%
  #layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 7, activation = 'softmax') %>% 
  compile(
    loss = 'mean_absolute_error',
    #optimizer = optimizer_adam(lr = 0.0001, decay = 1e-5),
    optimizer = optimizer_sgd(lr = 0.01, decay = 1e-4, momentum = 0.9, nesterov = TRUE),
    metrics = c('accuracy')     
  )
fit(model, CovarMatrix, TargetMatrix, epochs = 100, batch_size = 256, validation_split=0.3)
# really bad overfitting

# Try more layers
model = keras_model_sequential() %>% 
  layer_dense(units = 128, activation = 'relu', input_shape = c(67)) %>% 
  #layer_dropout(rate = 0.5) %>% 
  layer_batch_normalization() %>%
  layer_dense(units = 64, activation = 'relu') %>% 
  #layer_batch_normalization() %>%
  #layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 32, activation = 'relu') %>% 
  layer_batch_normalization() %>%
  #layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 7, activation = 'softmax') %>% 
  compile(
    loss = 'mean_absolute_error',
    optimizer = optimizer_adam(lr = 0.0001, decay = 1e-5),
    #optimizer = optimizer_sgd(lr = 0.01, decay = 1e-4, momentum = 0.9, nesterov = TRUE),
    metrics = c('accuracy')     
  )
fit(model, CovarMatrix, TargetMatrix, epochs = 100, batch_size = 256, validation_split=0.3)

preds = predict(model, ValidMatrix)
colnames(preds) = colnames(TargetMatrix)
Predictions = as.data.frame(preds*100)
AccuracyStatisticsPlots(Predictions, Data.val[,GetCommonClassNames()]) # 23.8% RMSE, 10.0 MAE, a slight bit better again
SCM(Predictions/100, Data.val[,GetCommonClassNames()]/100, plot=TRUE, totals=TRUE) # OA 65±2 kappa 0.54±0.02
NSE(unlist(Predictions)/100, unlist(Data.val[,GetCommonClassNames()]/100)) # 0.37
PlotHex(Predictions, Data.val[,GetCommonClassNames()], "Neural networks, 3 hidden layers (128&64&32), softmax, adam, NA to 0, no dropout")
PlotBox(Predictions, Data.val[,GetCommonClassNames()], main="Neural networks, 3 hidden layers (128&64&32), softmax, adam, NA to 0, no dropout", binpredicted=TRUE)
write.csv(Predictions, "../data/pixel-based/predictions/nn-3layers-softmax-adam-0na-nodropout.csv")

