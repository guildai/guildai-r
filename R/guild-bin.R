

find_python <- function() {
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

  system2(find_python(), c("-m", "venv", shQuote(venv)))
  py <- file.path(venv, "bin", if(is_windows()) "python.exe" else "python")
  system2(py, c("-m", "pip", "install", "--upgrade", "--no-user", "pip", "wheel", "setuptools"))
  system2(py, c("-m", "pip", "install", "--upgrade", "--no-user", "guildai"))
  file.path(venv, "bin", "guild")
}

find_guild <- function() {
  if(file.exists(guild <- Sys.which("guild")))
    return(as.vector(guild))
  if(file.exists(guild <- file.path(rappdirs::user_data_dir("r-guildai"), "bin", "guild")))
    return(as.vector(guild))
  install_guild()
}


guild <- function(cmd, ..., stdout = FALSE) {
  system2(normalizePath(find_guild()), c(cmd, shQuote(c(...))), stdout = stdout)
}


ls_runs <- function() {
  x <- guild("runs", "--json", stdout = TRUE)
  x <- jsonlite::parse_json(x, simplifyVector = TRUE)
  tibble::as_tibble(x)
}


is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}


training_run <- function(file = "train.R", flags = NULL) {
  guild("run", "--yes", file)
}

view_runs <- function() {
  guild("view")
}
