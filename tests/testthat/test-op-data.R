test_that("guild run", {

  local_project(test_files("basic.R"))

  guild_run("basic.R")

  expect(nrow(ls_runs()) == 1)
  expect_equal(nrow(ls_runs()), 1L)

})


test_that("rscript op data inference", {

  # blanket check that each file can be poked w/o error
  for(f in list.files(test_files(""), pattern = "\\.R$", full.names = TRUE)) {
    op <- capture.output(guildai:::emit_r_script_guild_data(f))
    expect_no_error(yaml::yaml.load(op))
  }

  # more granular checks about the output op data
  op <- r_script_guild_data(test_files("empty.R"))
  expect_length(op$flags, 0)
  expect_equal(basename(op$name), "empty.R")

  op <- r_script_guild_data(test_files("basic.R"))
  expect_length(op$flags, 2)
  expect_setequal(names(op$flags), c("x", "noise"))
  expect_equal(basename(op$name), "basic.R")

  op <- r_script_guild_data(test_files("flags-anno.R"))
  class(op) <- NULL # drop yaml class, keep tests simple
  expect_setequal(names(op$flags), c("x", "noise", "type", "init_phase"))
  expect_equal(op$flags$x$description, "`x` by any other name would smell as sweet.")
  expect_equal(op$flags$type$choices, c("bar", "baz", "foo"))
  expect_equal(op$flags$noise$min, 0)
  expect_equal(op$flags$noise$max, 1)
  expect_equal(op$flags$init_phase$type, "complex")

})


