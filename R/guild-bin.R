

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
#' # install_guild("https://api.github.com/repos/guildai/guildai/tarball/HEAD")
#' # install_guild(
#' #   guildai = "https://api.github.com/repos/guildai/guildai/tarball/HEAD",
#' #   python = reticulate::install_python())
#' #
install_guild <- function(guildai = "guildai", python = find_python()) {
  venv <- normalizePath(rappdirs::user_data_dir("r-guildai"), mustWork = FALSE)
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
                    file.path(rappdirs::user_data_dir("r-guildai"), "Scripts", "guild.exe")))
      return(normalizePath(as.vector(guild)))
  if (file.exists(guild <-
                  file.path(rappdirs::user_data_dir("r-guildai"), "bin", "guild")))
    return(normalizePath(as.vector(guild)))
  if (file.exists(guild <- Sys.which("guild")))
    return(normalizePath(as.vector(guild)))
  install_guild()
}



guild <- function(...,
                  stdout = "", stderr = "",
                  home = NULL, #home = Sys.getenv("GUILD_HOME", here::here(".guild")),
                  wait = TRUE) {

  args <- list(...)
  args <- rapply(args, function(x) {
    if (inherits(x, "AsIs") || all(grepl("^[[:alpha:]-]+$", x)))
      x
    else
      shQuote(x)
  })
  stopifnot(is.null(names(args)))
  # AsIs <- vapply(args, inherits, TRUE, "AsIs")
  # args[!AsIs] <- shQuote(args[!AsIs])

  ##? allow args like guild("--path" = r_sym)
  # for(nm in names(args))
  #   if(isTRUE(nzchar(nm)))
  #     args[[nm]] <- sprintf("%s=%s", nm, args[[nm]])

  args <- as.character(unlist(args))

  if(!is.null(home))
    args <- c("-H", shQuote(home), args)
  if(Sys.getenv("DEBUG") == "1")
    args <- c("-D", "5678", args)
  system2t(find_guild(), args,
          stdout = stdout, stderr = stderr,
          wait = wait)
}


.guild <- function(args, ...) {
  # convenience version that accepts args as a single string
  guild(unlist(strsplit(args, "\\s+", perl = TRUE)), ...)
}
