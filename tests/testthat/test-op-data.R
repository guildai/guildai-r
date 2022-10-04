test_that("guild run", {

  local_project(test_resource("basic.R"))
  guild_run("basic.R", echo = FALSE, wait = TRUE)
  expect_equal(nrow(ls_runs()), 1L)

})


test_that("rscript op data inference", {

  # blanket check that each file can be poked w/o error
  for(f in list.files(test_resource(),
                      pattern = "\\.R$",
                      full.names = TRUE)) {
    if(basename(f) == "train-flags-yml.R")
      next
    op <- capture.output(guildai:::emit_r_script_guild_data(f))
    expect_no_error(yaml::yaml.load(op))
  }

  # more granular checks about the output op data
  op <- r_script_guild_data(test_resource("empty.R"))
  expect_length(op$flags, 0)
  expect_equal(basename(op$name), "empty.R")

  op <- r_script_guild_data(test_resource("basic.R"))
  expect_length(op$flags, 2)
  expect_setequal(names(op$flags), c("x", "noise"))
  expect_equal(basename(op$name), "basic.R")

  op <- r_script_guild_data(test_resource("hash-pipe-anno.R"))
  class(op) <- NULL # drop yaml class, keep tests simple
  expect_setequal(names(op$flags), c("x", "noise", "type", "init_phase"))
  expect_equal(op$flags$x$description, "`x` by any other name would smell as sweet.")
  expect_equal(op$flags$type$choices, c("bar", "baz", "foo"))
  expect_equal(op$flags$noise$min, 0)
  expect_equal(op$flags$noise$max, 1)

  # TODO: guild does not support a "complex" type, so we have to lie
  #       here and tell guild it's a string.
  # expect_equal(op$flags$init_phase$type, "complex")
  expect_equal(op$flags$init_phase$type, "string")

  # frontmatter is passed through
  expect_equal(op$requires,
               list(list(file = "file.txt",
                         description = "File dependency")))

  expect_equal(op$requires,
               list(list(file = "file.txt",
                         description = "File dependency")))

  expect_equal(op$`output-scalars`,
               list(step = "step: (\\value)",
                    loss = "loss: (\\value)",
                    accuracy = "accuracy: (\\value)"))

  # test frontmatter parsing works the same w/o shebang
  writeLines(readLines(test_resource("hash-pipe-anno.R"))[-1],
             w_o_shebang <- tempfile(fileext = ".R"))
  op2 <- r_script_guild_data(w_o_shebang)
  class(op2) <- NULL
  op $exec <- op $name <-
  op2$exec <- op2$name <- NULL
  expect_identical(op, op2)

})


test_that("guild run w/ flags-dest: config:flags.yml", {

  local_project(test_resource("flags-from-yml.R", "flags.yml"))


  # sort_by_names <- function(x) x[order(names(x))]

  # confirm the defaults are in the run dir, flags.yml is resolved
  default_flags <- read_yaml("flags.yml")
  guild_run("flags-from-yml.R", wait = TRUE)
  run_observed_flags <- parse_yaml(.guild("cat --output", stdout = TRUE))
  expect_mapequal(default_flags, run_observed_flags)

  # confirm passing non-default flag b=TRUE
  # resolves a modified flags.yml in the rundir
  guild_run("flags-from-yml.R", flags = c(b = !default_flags$b), wait = TRUE)
  run_observed_flags <- parse_yaml(.guild("cat --output", stdout = TRUE))
  expect_mapequal(modifyList(default_flags, list(b = !default_flags$b)),
                  run_observed_flags)

  # because boolean flags are tricky, test the inverse default too
  default_flags$b <- TRUE
  print(default_flags, file = "flags.yml")
  # sanity check print.yaml()
  expect_identical(read_yaml("flags.yml"), default_flags)

  # test default
  guild_run("flags-from-yml.R", wait = TRUE)
  run_observed_flags <- parse_yaml(.guild("cat --output", stdout = TRUE))
  expect_mapequal(default_flags, run_observed_flags)

  # test non-default
  guild_run("flags-from-yml.R", flags = c(b = !default_flags$b), wait = TRUE)
  run_observed_flags <- parse_yaml(.guild("cat --output", stdout = TRUE))
  expect_mapequal(modifyList(default_flags, list(b = !default_flags$b)),
                  run_observed_flags)

  # TODO: Warning in readLines(file, warn = readLines.warn) :
  # incomplete final line found on 'flags.yml'

})


test_that("guild run w/ flags-dest: globals", {

  file <- "flags-from-globals.R"
  local_project(test_resource(file))


  guild_run(file, wait = TRUE)
  output <- expect_snapshot_guild_cat_last_output()

  invisible(capture.output(source(file, default_flags <- new.env())))
  default_flags <- as.list(default_flags)

  guild_run(file, flags = default_flags, wait = TRUE)
  output2 <-.guild("cat --output", stdout = TRUE)
  expect_identical(output, output2)

  flags <- list(
    i = 456L, f = 4.56, s = "Howdy Back", b = TRUE,
    s2 = "abc", s3 = "def", cx = 1+1i, cx1 = 2+2i
  )

  guild_run(file, flags = flags, wait = TRUE)
  expect_snapshot_guild_cat_last_output()

  guild_run(file, flags = list(s = "foo\nbar\nbaz"), wait = TRUE)
  expect_snapshot_guild_cat_last_output()

})


# sort_by_names <- function(x) x[order(names(x))]
#
#
#   args <- c(
#     # "/home/tomasz/opt/R-4.2.1/lib/R/bin/exec/R",
#     # "--quiet",
#     # "--no-save",
#     # "--no-restore-data",
#     # "--no-echo",
#     # "--no-restore",
#     # "-e",
#     # "guildai:::do_guild_run(\"./flags-from-globals.R\",~+~flags_dest~+~=~+~\"globals\",~+~echo~+~=~+~FALSE)",
#     # "--args",
#     "--b",
#     "false",
#     "--cx",
#     "0+0i",
#     "--cx1",
#     "1+1i",
#     "--f",
#     "1.123",
#     "--i",
#     "123",
#     "--s",
#     "Howdy Guild",
#     "--s2",
#     "foo",
#     "--s3",
#     "\"\\n    foobarbaz\\n    \""
#   )
#   browser()
