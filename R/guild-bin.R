

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


#' @export
install_guild <- function(python = NULL, guildai = NULL) {
  venv <- normalizePath(rappdirs::user_data_dir("r-guildai"), mustWork = FALSE)
  unlink(venv, recursive = TRUE)
  system2(python %||% find_python(), c("-m", "venv", shQuote(venv)))
  py <- file.path(venv, "bin", if(xfun::is_windows()) "python.exe" else "python")
  pip_install <- function(...)
    system2(py, c("-m", "pip", "install", "--upgrade", "--no-user", ...))
  pip_install("pip", "wheel", "setuptools")
  pip_install(if(is.null(guildai)) "guildai" else c('-e', guildai))
  normalizePath(file.path(venv, "bin", "guild"))
}

# install_guild("/usr/bin/python3", "~/guild/guildai")


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
  system2(find_guild(), args,
          stdout = stdout, stderr = stderr,
          wait = wait)
}
