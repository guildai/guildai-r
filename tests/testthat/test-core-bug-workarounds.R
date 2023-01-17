test_that("flag names that need quoting work", {
  file <- "tricky-flag-names.R"
  local_project(test_resource(file))
  # browser(); Sys.setenv(GUILD_DEBUG_R=1)

  guild_run(file)
  expect_snapshot_guild_cat_last_output()

  flags <- lapply(unclass(guildai:::r_script_guild_data(file)$flags),
                  function(x) x$default)
  guild_run(file, flags = lapply(flags, function(value) value + 100L))
  expect_snapshot_guild_cat_last_output()
})
