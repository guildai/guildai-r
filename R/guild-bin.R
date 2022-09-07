

find_python <- function() {
  # consider using pipenv or similar to make a truly stand-alone installation
  if(file.exists(python <- Sys.which("python3")))
    return(as.vector(python))
  if(file.exists(python <- Sys.which("python")))
    return(as.vector(python))
  stop(
"python executable not found. Please:
  -  download and install Python http://python.org/downloads/
  -  ensure it is on your PATH")
}


install_guild <- function() {
  venv <- normalizePath(rappdirs::user_data_dir("r-guildai"), mustWork = FALSE)
  unlink(venv, recursive = TRUE)
  system2(find_python(), c("-m", "venv", shQuote(venv)))
  py <- file.path(venv, "bin", if(xfun::is_windows()) "python.exe" else "python")
  system2(py, c("-m", "pip", "install", "--upgrade", "--no-user", "pip", "wheel", "setuptools"))
  system2(py, c("-m", "pip", "install", "--upgrade", "--no-user", "guildai"))
  normalizePath(file.path(venv, "bin", "guild"))
}

find_guild <- function() {
  if(file.exists(guild <- Sys.which("guild")))
    return(normalizePath(as.vector(guild)))
  if(file.exists(guild <- file.path(rappdirs::user_data_dir("r-guildai"), "bin", "guild")))
    return(normalizePath(as.vector(guild)))
  install_guild()
}


guild <- function(cmd, ..., stdout = "", stderr = "",
                  home = Sys.getenv("GUILD_HOME", here::here(".guild")),
                  wait = TRUE) {
  args <- shQuote(c("-H", home, cmd, c(...)))
  system2(find_guild(), args, stdout = stdout, stderr = stderr,
          wait = wait)
}
