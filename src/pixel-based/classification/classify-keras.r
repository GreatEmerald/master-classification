# Use Keras for a neural network instead of neuralnet

# Set environment variables
source("packrat/init.R")
# Interface with a local conda environment: this needs to be configured for each system
Sys.setenv("PATH" = paste0("/usr/local/cuda-10.0/bin:", Sys.getenv("HOME"), "/miniconda3/bin:", Sys.getenv("PATH")))
Sys.setenv(CUDA_HOME="/usr/local/cuda-10.0")
#Sys.setenv("LD_LIBRARY_PATH" = paste0(Sys.getenv("HOME"), "/miniconda3/envs/r-tensorflow/lib:", Sys.getenv("LD_LIBRARY_PATH")))
library(keras)

source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("pixel-based/utils/crossvalidation.r")
#registerDoParallel(cores=10)
source("pixel-based/utils/subpixel-confusion-matrix.r") # Replace with package eventually
source("utils/accuracy-statistics.r")

Data.df = LoadTrainingAndCovariates()
class(Data.df) = "data.frame"
Data.df = AddZeroValueColumns(Data.df)
Data.df = TidyData(Data.df)

ValidationData = LoadValidationAndCovariates()
class(ValidationData) = "data.frame"
ValidationData = TidyData(ValidationData)

TargetMatrix = as.matrix(Data.df[,GetCommonClassNames()])/100
CovarMatrix = as.matrix(apply(Data.df[,GetAllPixelCovars()], 2, scale))

model = keras_model_sequential() %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(313)) %>% 
  #layer_dropout(rate = 0.5) %>% 
  layer_batch_normalization() %>%
  layer_dense(units = 256, activation = 'relu') %>% 
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 256, activation = 'relu') %>% 
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 7, activation = 'sigmoid') %>% 
  compile(
    loss = 'mean_absolute_error',
    optimizer = optimizer_adam(lr = 0.0001, decay = 1e-5), #optimizer_sgd(lr = 0.01, decay = 1e-4, momentum = 0.9, nesterov = TRUE),
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


history = fit(model, CovarMatrix, TargetMatrix, epochs = 300, batch_size = 256, validation_split=0.3)
              # callbacks=list(callback_learning_rate_scheduler(schedule),callback_reduce_lr_on_plateau(monitor = "val_loss", factor = 0.1,patience = 10)))
plot(history)

# evaluate
#score <- model %>% evaluate(x_test, y_test, batch_size = 128)
#score

ValidMatrix = as.matrix(ValidationData)
preds = predict(model, CovarMatrix)
colnames(preds) = colnames(TargetMatrix)
head(preds)
rowSums(preds)

round(head(TargetMatrix), 2)
round(head(preds), 2)

library(ggplot2)

ggplot(data.frame(Prediction=c(preds), Truth=c(TargetMatrix)), aes(Prediction, Truth)) +
    geom_hex() +
    scale_fill_distiller(palette=7, trans="log") + #log scale
    geom_abline(slope=1, intercept=0) + ggtitle("Neural Networks, full model")

AccuracyStatisticsPlots(as.data.frame(preds), as.data.frame(TargetMatrix)) # 19.5% RMSE
SCM(as.data.frame(preds), as.data.frame(TargetMatrix), plot=TRUE, totals=TRUE, scale = TRUE)

