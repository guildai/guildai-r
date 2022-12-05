#| echo: off
#| description: Test a model
#| requires:
#|   - operation: train.R
#|     select: model.rds

model <- readRDS("model.rds")

summary(model)
