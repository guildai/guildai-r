

find_python <- function() {
  # consider using pipenv or similar to make a truly stand-alone installation

  for (python in as.vector(c(
    "/usr/bin/python3",
    Sys.which("python3"),
    Sys.which("python")
    # TODO: search also in default python locations on windows
    # TODO: do we really want to use system python on mac?
    #       Maybe look in /usr/bin/local first?
  )))
    if(file.exists(python))
      return(python)

  stop(
"python executable not found. Please:
  -  download and install Python http://python.org/downloads/
  -  ensure it is on your PATH")
}

system2t <- function (command, args, ...) {
  if(Sys.getenv("DEBUGR") == "1") {
    cl <- as.call(c(list(quote(system2t), command, args, ...)))
    message(paste("R>", deparse1(cl)))
    message(paste("sys+", shQuote(command), paste0(args, collapse = " ")))
  }
  system2(command, args, ...)
}





#' Install guildai core
#'
#' @param guildai character vector passed directly to `pip install`.
#' @param python path to python binary, used to create a private venv.
#'
#' @return path to the guild binary
#' @export
#'
#' @examples
#' # install_guild(c("-e", "~/guild/guildai"))
#' # install_guild("~/guildai", reticulate::install_python())
#' # install_guild("https://api.github.com/repos/guildai/guildai/tarball/HEAD")
#' # install_guild(
#' #   guildai = "https://api.github.com/repos/guildai/guildai/tarball/HEAD",
#' #   python = reticulate::install_python())
#' #
install_guild <- function(guildai = "guildai", python = find_python()) {
  venv <- normalizePath(rappdirs::user_data_dir("r-guildai", NULL), mustWork = FALSE)
  unlink(venv, recursive = TRUE)
  python <- normalizePath(python)
  system2(python, c("-m", "venv", shQuote(venv)))
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
                    file.path(rappdirs::user_data_dir("r-guildai", NULL), "Scripts", "guild.exe")))
      return(normalizePath(as.vector(guild)))
  if (file.exists(guild <-
                  file.path(rappdirs::user_data_dir("r-guildai", NULL), "bin", "guild")))
    return(normalizePath(as.vector(guild)))
  if (file.exists(guild <- Sys.which("guild")))
    return(normalizePath(as.vector(guild)))
  install_guild()
}


.guild <- function(args, ...) {
  # convenience version that accepts args as a single string
  guild(unlist(strsplit(args, "\\s+", perl = TRUE)), ...)
}
