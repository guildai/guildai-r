

find_python <- function() {
  # consider using pipenv or similar to make a truly stand-alone installation

  for (python in as.vector(c(
    "/usr/bin/python3",
    Sys.which("python3"),
    Sys.which("python")
  ))
  )
    if(file.exists(python))
      return(python)

  stop(
"python executable not found. Please:
  -  download and install Python http://python.org/downloads/
  -  ensure it is on your PATH")
}

system2t <- function (command, args, ...) {
  message(paste("+", shQuote(command), paste0(args, collapse = " ")))
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
#  # install_guild("https://api.github.com/repos/guildai/guildai/tarball/HEAD")
install_guild <- function(guildai = "guildai", python = find_python()) {
  venv <- normalizePath(rappdirs::user_data_dir("r-guildai"), mustWork = FALSE)
  unlink(venv, recursive = TRUE)
  system2(python %||% find_python(), c("-m", "venv", shQuote(venv)))
  python <- file.path(venv, "bin", if(is_windows()) "python.exe" else "python")
  pip_install <- function(...)
    system2t(python, c("-m", "pip", "install", "--upgrade", "--no-user", ...))
  pip_install("pip", "wheel", "setuptools")
  pip_install(guildai)
  normalizePath(file.path(venv, "bin", "guild"))
}






find_guild <- function() {
  if(file.exists(guild <- file.path(rappdirs::user_data_dir("r-guildai"), "bin", "guild")))
    return(normalizePath(as.vector(guild)))
  if(file.exists(guild <- Sys.which("guild")))
    return(normalizePath(as.vector(guild)))
  install_guild()
}


guild <- function(cmd, ..., stdout = "", stderr = "",
                  home = NULL, #home = Sys.getenv("GUILD_HOME", here::here(".guild")),
                  wait = TRUE) {

  args <- shQuote(c(cmd, c(...)))
  if(!is.null(home))
    args <- c("-H", shQuote(home), args)
  if(Sys.getenv("DEBUG") == "1")
    args <- c("-D", "5678", args)
  system2t(find_guild(), args,
          stdout = stdout, stderr = stderr,
          wait = wait)
}


.guild <- function(line, ...) {
  # convenience version that accepts all args on a single line
  args <- strsplit(line, " ", fixed = TRUE)[[1]]
  do.call(guild, c(as.list(args), ...))
}
