

pkg_file <- function(...) {
  out <- vapply(list(...),
         function(p) system.file(p, package = "guildai"),
         "", USE.NAMES = FALSE)
  paste0(".", str_drop_prefix(out, getwd()))
}

test_resource <-  function(...) {
  test_path("resources", ...)
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
    cat("wd:", getwd(), "\n")
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


# frame <- parent.frame()
# if(identical(frame, globalenv()))
#   reg.finalizer(globalenv(), #withr::deferred_run, TRUE)
# function(e) {
#    rstudioapi::sendToConsole(sprintf("setwd('%s')", owd),
#                              execute = FALSE)
#     withr::deferred_run(e)
#   })

# restore <- function() {
#
# }
# browser()

# eval(bquote(withr::defer({
#   cat("Running finalizer!")
#   setwd(.(owd))
#   cat("getwd(): ", getwd(), "\n")
#   Sys.unsetenv("GUILD_HOME")
# }, envir = .(frame))))
