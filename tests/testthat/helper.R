

pkg_file <- function(...) {
  out <- vapply(list(...),
         function(p) system.file(p, package = "guildai"),
         "", USE.NAMES = FALSE)
  paste0(".", str_drop_prefix(out, getwd()))
}

test_resource <-  function(...) {
  vapply(list(...), function(p)  test_path("resources", p), "",
         USE.NAMES = FALSE)

}


expect_no_error <- expect_error
formals(expect_no_error)$regexp <- NA



# cat("Using guild:", guildai:::find_guild(), "\n")


local_project <- function(files, envir = parent.frame(), name = NULL) {
  # Similar in semantics to all the withr::local_* functions
  # It changes the working directory to a temporary directory w/ copies of the
  # requested files present. Exiting the `envir` scope switches back to the old working directory.
  # The directory is automatically cleaned up when the R session terminates.
  files <- normalizePath(files)
  dir.create(nwd <- tempfile(paste0(
    c("guildai-test-project", name, ""), collapse = "-")))

  withr::defer_parent({
    setwd(owd)
    if(is.na(old_guild_home))
      Sys.unsetenv("GUILD_HOME")
    else
      Sys.setenv("GUILD_HOME" = old_guild_home)
  })

  owd <- setwd(nwd)
  file.copy(files, basename(files))

  old_guild_home <- Sys.getenv("GUILD_HOME", NA_character_)
  Sys.setenv("GUILD_HOME" = file.path(getwd(), ".guild"))
  invisible()
}


# don't echo during tests unless interactively
if(!interactive())
  formals(guild_run)$echo <- FALSE

expect_snapshot_guild_cat_last_output <- function() {
  output <- guild("cat --output", stdout = TRUE)
  expect_snapshot_output(writeLines(output))
  invisible(output)
}



# TODO: does guild have away to distinguish between stdout and stderr output?

ll <- function(..., all.files = TRUE, recursive = TRUE, no.. = TRUE) {
  list.files(..., all.files = all.files, recursive = recursive, no.. = no..)
}

# need these methods in tests, but don't export them for users
registerS3method("print", "yaml", print.yaml)
registerS3method("$", "yaml", `$.yaml`)
registerS3method("[[", "yaml", `[[.yaml`)
registerS3method("[", "yaml", `[.yaml`)
registerS3method("str", "yaml", str.yaml)


if(interactive() && dir.exists("~/guild/fashion-mnist"))
  Sys.setenv("GUILD_HOME" = path.expand("~/guild/fashion-mnist/.guild"))
