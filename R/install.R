

find_python <- function() {

  for (python in as.vector(c(
    "/usr/bin/python3",
    Sys.which("python3"),
    Sys.which("python")
    # TODO: do we really want to use system python on mac?
    #       Maybe look in /usr/bin/local first?
  )))
    if(file.exists(python))
      return(python)

  if (is_windows() &&
      file.exists(python <- find_python_from_registry()))
    return(python)


  stop(
"python executable not found. Please:
  -  download and install Python http://python.org/downloads/
  -  ensure it is on your PATH")
}

find_python_from_registry <- function() {
  for (hive in c("HCU", "HLM")) {
    entries <- utils::readRegistry("SOFTWARE\\Python\\PythonCore",
                                   hive = hive,
                                   maxdepth = 3)
    for (en in entries) {
      tryCatch({
        python <- en$InstallPath$ExecutablePath
        if (file.exists(python))
          return(python)
      }, error = identity)
    }
  }
  invisible()
}




#' Install guildai core
#'
#' This installs the `guild` executable for use by the R package.
#'
#' @param guildai Character vector of arguments passed directly to `pip
#'   install`. To install the release version of guildai, this can be
#'   `"guildai"`.
#' @param python Path to a python binary, used to create a private venv.
#'
#' @return path to the guild binary
#' @export
#'
#' @examples
#' # install_guild(c("-e", "~/guild/guildai"))
#' # install_guild("~/guild/guildai", reticulate::install_python())
#' # install_guild("https://api.github.com/repos/guildai/guildai/tarball/HEAD")
#' # install_guild(
#' #   guildai = "https://api.github.com/repos/guildai/guildai/tarball/HEAD",
#' #   python = reticulate::install_python())
install_guild <-
  function(guildai = "https://api.github.com/repos/guildai/guildai/tarball/HEAD",
           python = find_python()) {
  venv <- normalizePath(rappdirs::user_data_dir("r-guildai", NULL), mustWork = FALSE)
  unlink(venv, recursive = TRUE)
  python <- normalizePath(python)
  system2t(python, c("-m", "venv", shQuote(venv)))
  python <- if (is_windows())
   file.path(venv, "Scripts", "python.exe", fsep = "\\") else
   file.path(venv, "bin", "python")
  pip_install <- function(...)
    system2t(python, c("-m", "pip", "install", "--upgrade", "--no-user", ...))
  pip_install("pip", "wheel", "setuptools")
  pip_install(guildai)
  if (is_windows())
    file.path(venv, "Scripts", "guild.exe", fsep = "\\") else
    file.path(venv, "bin", "guild")
}



find_guild <- function() {
  if (is_windows())
    if (file.exists(guild <-
                    file.path(rappdirs::user_data_dir("r-guildai", NULL),
                              "Scripts", "guild.exe")))
      return(normalizePath(as.vector(guild)))
  if (file.exists(guild <-
                  file.path(rappdirs::user_data_dir("r-guildai", NULL), "bin", "guild")))
    return(normalizePath(as.vector(guild)))
  if (file.exists(guild <- Sys.which("guild")))
    return(normalizePath(as.vector(guild)))
  install_guild()
}


#' Install guild for usage in the Terminal
#'
#' This function makes available the `guild` executable installed by
#' `install_guild()` for usage in the Terminal.
#'
#' @param dest Directory where to place the `guild` executable. This should be a
#'   location on the `PATH`.
#' @param completions Whether to also install shell completion helpers.
#'
#' @return path to the installed guild executable, invisibly.
#' @export
install_guild_cli <- function(dest = "~/bin", completions = TRUE) {
  if(!dir.exists(dest))
    dir.create(dest)

  guild_exe <- find_guild()
  link <- file.path(dest, basename(guild_exe))
  unlink(link)
  message("Creating symlink: '", link, "' -> '", guild_exe, "'")
  file.symlink(guild_exe, link)
  if (completions)
    guild("completion --install", "--shell" = basename(Sys.getenv("SHELL")))

  paths <- normalizePath(
    strsplit(Sys.getenv("PATH"), .Platform$path.sep, fixed = TRUE)[[1]],
    mustWork = FALSE)
  if (!normalizePath(dest) %in% paths)
    warning(sprintf("'%s' is not on the PATH.", dest))

  if (Sys.which("guild") != path.expand(link)) {
    warning("A different 'guild' executable is first on the PATH: ",
            "'", Sys.which("guild"), "'")
  }

  invisible(link)
}

