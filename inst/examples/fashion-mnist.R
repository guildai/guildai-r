library(keras)

# Prepare data ----
fashion_mnist <- dataset_fashion_mnist()

c(train_images, train_labels) %<-% fashion_mnist$train
c(test_images, test_labels) %<-% fashion_mnist$test

train_images <- train_images / 255
 test_images <- test_images / 255

# Define model ----

units <- 64

model <- keras_model_sequential(input_shape = c(28, 28))
model %>%
  layer_flatten() %>%
  layer_dense(units = units, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'softmax')


learning_rate <- 0.001

model %>% compile(
  optimizer = optimizer_adam(learning_rate),
  loss = 'sparse_categorical_crossentropy',
  metrics = c('accuracy')
)

model

# Fit model ----

batch_size <- 32
epochs <- 20

.fast <- TRUE
if (.fast) {
  n <- 1:20
  train_images %<>% { .[n, ,] }
  test_images %<>% { .[n, ,] }
  test_labels %<>% { .[n] }
  train_labels %<>% { .[n] }
  epochs <- 2
}

history <- model %>%
  fit(train_images, train_labels,
      validation_split = 0.2,
      batch_size = batch_size,
      epochs = epochs,
      verbose = 2)

plot(history)

# Evaluate model ----

score <- model %>%
  evaluate(test_images, test_labels,
           verbose = 0) %>%
  as.list()

cat('test_loss:', score$loss, "\n")
cat('test_accuracy:', score$accuracy, "\n")

# save_model_tf(model, "model.keras")
# saveRDS(history, "history.rds")
