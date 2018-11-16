# Use Keras for a neural network instead of neuralnet

# Set environment variables
source("packrat/init.R")
# Interface with a local conda environment: this needs to be configured for each system
Sys.setenv("PATH" = paste0(Sys.getenv("HOME"), "/miniconda3/bin:", Sys.getenv("PATH")))
#Sys.setenv("LD_LIBRARY_PATH" = paste0(Sys.getenv("HOME"), "/miniconda3/envs/r-tensorflow/lib:", Sys.getenv("LD_LIBRARY_PATH")))
library(keras)

source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("pixel-based/utils/crossvalidation.r")
registerDoParallel(cores=10)
source("pixel-based/utils/subpixel-confusion-matrix.r") # Replace with package eventually
source("utils/accuracy-statistics.r")

Data.df = LoadTrainingAndCovariates()
class(Data.df) = "data.frame"
Data.df = AddZeroValueColumns(Data.df)
Data.df = TidyData(Data.df)

TargetMatrix = as.matrix(Data.df[,GetIIASAClassNames(TRUE)])/100
CovarMatrix = as.matrix(apply(Data.df[,GetAllPixelCovars()], 2, scale))

model = keras_model_sequential() %>% 
  layer_dense(units = 128, activation = 'relu', input_shape = c(32)) %>% 
  #layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 128, activation = 'relu') %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 10, activation = 'softmax') %>% 
  compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_sgd(lr = 0.0001, decay = 1e-4, momentum = 0.9, nesterov = TRUE),
    metrics = c('accuracy')     
  )

# train
history = fit(model, CovarMatrix, TargetMatrix, epochs = 500, batch_size = 128)
plot(history)

# evaluate
score <- model %>% evaluate(x_test, y_test, batch_size = 128)
score

preds = predict(model, CovarMatrix)
colnames(preds) = colnames(TargetMatrix)
head(preds)
rowSums(preds)

library(ggplot2)

ggplot(data.frame(Prediction=c(preds), Truth=c(TargetMatrix)), aes(Prediction, Truth)) +
    geom_hex() +
    scale_fill_distiller(palette=7, trans="log") + #log scale
    geom_abline(slope=1, intercept=0) + ggtitle("Neural Networks, full model")

AccuracyStatisticsPlots(as.data.frame(preds), as.data.frame(TargetMatrix)) # 15.4% RMSE, probably overfitting
SCM(as.data.frame(preds), as.data.frame(TargetMatrix), plot=TRUE, totals=TRUE)

