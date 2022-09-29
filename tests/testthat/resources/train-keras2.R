
library(keras)
library(listarrays)

c(c(train_images, train_labels), .) %<-% dataset_mnist()
train_images <- array_reshape(train_images / 255,
                              c(60000, 28 * 28))

batch_size <- 128L
units_1 = 512L


train_images %<>% extract_rows(1:100)
train_labels %<>% extract_rows(1:100)

model <- keras_model_sequential(input_shape = c(784)) %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_dense(units = 10, activation = "softmax")

model %>% compile(optimizer = optimizer_rmsprop(1),
                  loss = "sparse_categorical_crossentropy",
                  metrics = "accuracy")

history <- model %>% fit(train_images, train_labels,
                         epochs = 2, batch_size = batch_size,
                         validation_split = 0.2,
                         callbacks = callback_tensorboard("logs/"))


model
