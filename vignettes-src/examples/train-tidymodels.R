library(tidymodels)
library(butcher)
library(bestNormalize)
library(sessioninfo)
set.seed(1234)

# Boilerplate options -------------------------------------------------
tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

# Setup and partition data --------------------------------------------
data(ames)

ames <-
  ames %>%
  mutate(Sale_Price = log10(Sale_Price)) %>%
  select(-contains("_Cond"))

ames_split <- initial_split(ames, strata = Sale_Price)
ames_not_test <- training(ames_split)
ames_test <- testing(ames_split)

ames_rs <- validation_split(ames_not_test, strata = Sale_Price)
ames_train <- analysis(ames_rs$splits[[1]])
ames_val <- assessment(ames_rs$splits[[1]])

# Flags ---------------------------------------------------------------

#| description: spline degrees of freedom for longitude
#| min: 2
#| max: 50
longitude_df <- 40L

#| description: spline degrees of freedom for latitude
#| min: 2
#| max: 50
latitude_df <- 40L

#| description: L2 penalty
#| min: 0.0
#| max: 1.0
pen_val <- 0.001

#| description: Mixture of L1 and L2 penalty
#| min: 0.0
#| max: 1.0
mix_val <- 1.0

# Recipe and Workflow -------------------------------------------------

ames_rec <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
  step_other(MS_SubClass, MS_Zoning, Neighborhood, threshold = 0.05) %>%
  step_orderNorm(Lot_Area, ends_with("_SF"), Gr_Liv_Area) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_interact(~ starts_with("Central"):Year_Built) %>%
  # NOTE We splice the values in via !! so that the step uses their values
  # and not a reference to global variables (a very bad idea)
  step_spline_natural(Longitude, deg_free = !!longitude_df) %>%
  step_spline_natural(Latitude, deg_free = !!latitude_df)

glmn_spec <-
  linear_reg(penalty = pen_val, mixture = mix_val) %>%
  set_engine("glmnet")

ames_wflow <-
  workflow() %>%
  add_model(glmn_spec) %>%
  add_recipe(ames_rec)

ames_fit <- fit(ames_wflow, data = ames_train)
ames_fit

# fit -----------------------------------------------------------------

glmnet_fit <-
  ames_fit %>%
  extract_fit_engine()

glmnet_pred <-
  glmnet_fit %>%
  coef(s = pen_val) %>%
  apply(2, function(x) sum(x != 0))

glmnet_fit %>% autoplot(best_penalty = pen_val)


# Validation set results ----------------------------------------------

ames_pred <- augment(ames_fit, ames_val)

ames_pred %>%
  ggplot(aes(Sale_Price, .pred)) +
  geom_abline(col = "green", lty = 2) +
  geom_point(alpha = 1 / 3) +
  coord_obs_pred() +
  labs(x = "Observed (log-10)", y = "Predicted (log-10)")

val_results <- ames_pred %>% metrics(Sale_Price, .pred)

cat('validation_rmse:', val_results$.estimate[1], "\n")
cat('validation_R2:', val_results$.estimate[2], "\n")
cat('num_predictors:', glmnet_pred, "\n")

# Save object ---------------------------------------------------------

# butcher the objects to make their install sizes smaller
ames_fit <- butcher(ames_fit)

# Save objects in the current working directory
save(ames_fit, val_results, file = ".RData",
     compress = TRUE)

# ---------------------------------------------------------------------

session_info()
