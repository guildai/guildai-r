#| output-scalars:
#|   - step: '\[(\step)]'
#|   - '(\key):(\value)'

library(xgboost)
data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test

dtrain <- xgb.DMatrix(data = train$data, label=train$label)
dtest <- xgb.DMatrix(data = test$data, label=test$label)

watchlist <- list(train=dtrain, test=dtest)

nrounds = 20
eta = .2 # 1

bst <- xgb.train(
  data = dtrain,
  max.depth = 2,
  eta = eta,
  nthread = 2,
  nrounds = nrounds,
  watchlist = watchlist,
  eval.metric = "error",
  eval.metric = "logloss",
  objective = "binary:logistic"
)

# save model to binary local file
xgb.save(bst, "xgboost.model")
