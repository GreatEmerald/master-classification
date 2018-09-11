# Use Keras for a neural network instead of neuralnet

# Set environment variables
source("packrat/init.R")
# Interface with a local conda environment: this needs to be configured for each system
Sys.setenv("PATH" = paste0(Sys.getenv("HOME"), "/miniconda3/bin:", Sys.getenv("PATH")))
#Sys.setenv("LD_LIBRARY_PATH" = paste0(Sys.getenv("HOME"), "/miniconda3/envs/r-tensorflow/lib:", Sys.getenv("LD_LIBRARY_PATH")))
library(keras)

mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y
# reshape
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))
# rescale
x_train <- x_train / 255
x_test <- x_test / 255

y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')
summary(model)

compile(model, loss = 'categorical_crossentropy', optimizer = optimizer_rmsprop(), metrics = c('accuracy'))

history <- fit(model, x_train, y_train, epochs = 30, batch_size = 128, validation_split = 0.2)
plot(history)
evaluate(model, x_test, y_test)
predict_classes(model, x_test)

## Fuzzy classification

# generate dummy data
x_train <- matrix(runif(1000*20), nrow = 1000, ncol = 20)

y_train <- runif(1000, min = 0, max = 9) %>% 
  round() %>%
  matrix(nrow = 1000, ncol = 1) %>% 
  to_categorical(num_classes = 10)

x_test  <- matrix(runif(100*20), nrow = 100, ncol = 20)

y_test <- runif(100, min = 0, max = 9) %>% 
  round() %>%
  matrix(nrow = 100, ncol = 1) %>% 
  to_categorical(num_classes = 10)

# create model
model <- keras_model_sequential()

# define and compile the model
model %>% 
  layer_dense(units = 64, activation = 'relu', input_shape = c(20)) %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 64, activation = 'relu') %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 10, activation = 'softmax') %>% 
  compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_sgd(lr = 0.01, decay = 1e-6, momentum = 0.9, nesterov = TRUE),
    metrics = c('accuracy')     
  )

# train
history = fit(model, x_train, y_train, epochs = 50, batch_size = 128)
plot(history)

# evaluate
score <- model %>% evaluate(x_test, y_test, batch_size = 128)
score

preds = predict(model, x_test)
head(preds)
rowSums(preds)

# How about fuzzy
