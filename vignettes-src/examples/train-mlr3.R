library(mlr3)

# create learning task
task_penguins = as_task_classif(species ~ ., data = palmerpenguins::penguins)
task_penguins

cp <- 0.01

# load learner and set hyperparameter
learner = lrn("classif.rpart", cp = cp)

# train/test split
split = partition(task_penguins, ratio = 0.67)

# train the model
learner$train(task_penguins, split$train_set)

# predict data
prediction = learner$predict(task_penguins, split$test_set)

# calculate performance
prediction$confusion

measure = msr("classif.acc")
score <- prediction$score(measure)

# output scalars for guild
writeLines(sprintf(
  "%s: %f",
  names(score), score[[1]]))
