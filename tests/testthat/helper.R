

pkg_files <- function(...) {
  vapply(list(...),
         function(p) system.file(p, package = "guildai"),
         "", USE.NAMES = FALSE) |> fs::path_rel()
}

test_files <-  function(...) {
  vapply(list(...), function(p)
    system.file(file.path("tests", "testthat", "resources", p),
                package = "guildai"),
    "", USE.NAMES = FALSE) |> fs::path_rel()
}


expect_no_error <- expect_error
formals(expect_no_error)$regexp <- NA




cat("Using guild:", guildai:::find_guild(), "\n")


local_project <- function(files, envir = parent.frame(), name = NULL) {
  # Similar in semantics to all the withr::local_* functions
  # It changes the working directory to a temporary directory w/ copies of the
  # requested files present. Exiting the `envir` scope switches back to the old working directory.
  # The directory is automatically cleaned up when the R session terminates.
  files <- normalizePath(files)
  # if(Sys.getenv("DEBUG") == "1")
    message("Copying files:", paste0("\n- ", files))
  dir.create(nwd <- tempfile(paste0(
    c("guildai-test-project", name, ""), collapse = "-")))
  owd <- setwd(nwd)
  # if(Sys.getenv("DEBUG") == "1")
    message("wd: ", getwd())
  file.copy(files, ".", recursive = TRUE)
  old_guild_home <- Sys.getenv("GUILD_HOME", NA_character_)
  Sys.setenv("GUILD_HOME" = file.path(getwd(), ".guild"))

  withr::defer_parent({
    setwd(owd)
    cat("wd:", getwd(), "\n")
    if(is.na(old_guild_home))
      Sys.unsetenv("GUILD_HOME")
    else
      Sys.setenv("GUILD_HOME" = old_guild_home)
  })
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
