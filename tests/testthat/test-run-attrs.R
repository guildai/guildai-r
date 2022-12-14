

test_that("seed is recorded correctly", {
  withr::local_preserve_seed()
  local_project(test_resource("random.R"))
  for(i in 1:3)
    guild_run("random.R")
  runs <- runs_info()
  observed <- runs_scalars()$value
  reproduced <- sapply(runs$dir, function(d) {
    rs <- yaml::read_yaml(file.path(d, ".guild/attrs/random_seed"))
    set.seed(rs)
    runif(1)
  }, USE.NAMES = FALSE)

  ## tf events store floats, not doubles
  # np <- reticulate::import("numpy")
  # format(1 - np$nextafter(1, 0, dtype = "float32"),
  #        scientific = T, digits = 2)
  expect_equal(reproduced, observed,
               tolerance = 6e-8)
})
