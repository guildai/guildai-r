
test_that("ops can be defined in guild.yml", {
  local_project(test_resource("basic.R"))
  # Sys.setenv("GUILD_DEBUG_R" = 1); browser()
  print.yaml(
    list("basic.R" = list("description" = "a description about basic.R")),
    file = "guild.yml")
  guild_run("basic.R")
  expect_equal(nrow(runs_info()), 1L)

  x = guild("ops", stdout = TRUE)
  expect_match(x, "a description about basic.R", fixed = TRUE)


  x = guild("run --help-op", stdout = TRUE)
  x = paste0(x, collapse = "")
  expect_match(x, "a description about basic.R", fixed = TRUE)
  expect_match(x, "noise\\s+\\(default is 0.2\\)")
})
