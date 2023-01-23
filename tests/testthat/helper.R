

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



local_project <- function(files, envir = parent.frame(), name = NULL,
                          delete_on_success = !interactive()) {
  # Similar in semantics to all the withr::local_* functions. It changes the
  # working directory to a temporary directory w/ copies of the requested
  # files present. Exiting the `envir` scope switches back to the old
  # working directory. The directory is automatically cleaned up when the R
  # session terminates.
  files <- normalizePath(files, mustWork = TRUE)
  dir.create(nwd <- tempfile(paste0(
    c("guildai-test-project", name, ""), collapse = "-")))

  withr::defer_parent({
    setwd(owd)
    if(is.na(old_guild_home))
      Sys.unsetenv("GUILD_HOME")
    else
      Sys.setenv("GUILD_HOME" = old_guild_home)
    # clean up if running non-interactively and scope finished without error
    if(delete_on_success)
       # &&
       # !identical(returnValue(quote(.__no_return_value_sentinal__.)),
       #            quote(.__no_return_value_sentinal__.)))
      unlink(nwd, recursive = TRUE) else
        cat("Directory remaining:", nwd, "\n")
  })

  owd <- setwd(nwd)
  file.copy(files, basename(files))

  guild_home <- file.path(getwd(), ".guild")
  dir.create(guild_home)
  old_guild_home <- Sys.getenv("GUILD_HOME", NA_character_)
  Sys.setenv("GUILD_HOME" = file.path(getwd(), ".guild"))
  invisible()
}



if(interactive()) {

  options(guildai.run_as_job = FALSE)
  message("Using guild: ", guildai:::find_guild())

} else { #if(!interactive()) {

  # don't echo during tests unless interactively
  formals(guild_run)$echo <- FALSE
  Sys.setenv(LOG_LEVEL = 30)

}

expect_snapshot_guild_cat_last_output <- function() {
  output <- guild("cat --output", stdout = TRUE)
  expect_snapshot_output(writeLines(output))
  invisible(output)
}


ll <- function(..., all.files = TRUE, recursive = TRUE, no.. = TRUE) {
  list.files(..., all.files = all.files, recursive = recursive, no.. = no..)
}

# need these methods in tests, but don't export them for users
registerS3method("print", "yaml", print.yaml)
registerS3method("$", "yaml", `$.yaml`)
registerS3method("[[", "yaml", `[[.yaml`)
registerS3method("[", "yaml", `[.yaml`)
registerS3method("str", "yaml", str.yaml)


import_from <- function(x, ..., .into = parent.frame()) {
  ns <- getNamespace(substitute(x))
  dots <- eval(substitute(alist(...)))
  if(!is.null(names(dots))) stop("renaming not implemented")
  for(obj in dots) {
    stopifnot(is.symbol(obj))
    assign(as.character(obj), eval(obj, ns), .into)
  }
}

import_from(withr,
            local_envvar, with_envvar,
            local_path, with_path)

