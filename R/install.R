

find_python <- function() {
  for (python in as.vector(c(
    "/usr/bin/python3",
    Sys.which("python3"),
    Sys.which("python")
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
#' This installs the `guild` executable for use by the R package. It creates
#' an isolated python virtual environment private to the R package and installs
#' guildai into it. Repeated calls to `install_guild()` result in a
#' fresh installation.
#'
#' It requires that a suitable python version is
#' available on the system.
#'
#' @note `install_guild()` installs guild as an isolated VM. For guild to
#' run a python operation, the python package `guildai` must be installed
#' in the python library where it will be used, E.g., with `pip install guildai`
#' or `reticulate::py_install()`.
#'
#' @param guildai Character vector of arguments passed directly to `pip
#'   install`. To install the release version of guildai, this can be
#'   `"guildai"`. Special values of `"release"` and `"dev"` are also accepted.
#' @param python Path to a python binary, used to create a private isolated venv.
#'
#' @return path to the `guild` executable
#' @export
#'
#' @examples
#' \dontrun{
#' ## Install release version:
#' install_guild()
#'
#' ## Install release version using a specific python
#' # path_to_python <- reticulate::install_python() # path to python executable
#' install_guild("guildai", python = path_to_python)
#'
#' ## Install development version
#' install_guild(guildai = "dev", python = path_to_python)
#'
#' ## Install development version from URL
#'  install_guild(
#'    guildai = "https://api.github.com/repos/guildai/guildai/tarball/HEAD",
#'    python = path_to_python)
#'
#' ## Install local development version:
#' install_guild(c("-e", "~/guild/guildai"))
#' }
install_guild <-
  function(guildai = "guildai",
           python = find_python()) {

  # force args before unlinking, in case forcing them causes an error
  force(guildai); force(python)

  venv <- normalizePath(rappdirs::user_data_dir("r-guildai", NULL),
                        mustWork = FALSE)
  unlink(venv, recursive = TRUE, force = TRUE)

  message("Installing guildai core.")
  system2t(python, c("-m", "venv", shQuote(venv)), echo_cmd = TRUE)
  python <- if (is_windows())
   file.path(venv, "Scripts", "python.exe", fsep = "\\") else
   file.path(venv, "bin", "python")

  if (identical(length(guildai), 1L)) {
    guildai <- switch(guildai,
        "development" = ,
        "dev" = "https://api.github.com/repos/guildai/guildai/tarball/HEAD",
        "release" = ,
        "rel" = "guildai",
        guildai
      )
  }

  pip_install <- function(...)
    system2t(python, c("-Im", "pip", "install",
                       "--no-user",
                       "--no-warn-script-location",
                       "--disable-pip-version-check",
                       ...),
             echo_cmd = TRUE)

  pip_install("--ignore-installed", "pip", "wheel", "setuptools")
  if('-e' %in% guildai)
    guildai <- c("--no-use-pep517", guildai)
  pip_install(guildai)

  # write out a ._pth file to ensure this python installation is isolated.
  # Meaning, user set VIRTUAL_ENV, PYTHONPATH, and similar don't
  # leak into the guild core internal python runtime.
  isolated_sys_path <-
    system2(python, c("-I", "-c",
                      shQuote("import sys; [print(p) for p in sys.path]")),
            stdout = TRUE)
  python._pth <- sprintf("%s._pth", sub("\\.exe$", "", python))
  message("Writing: ", python._pth)
  writeLines(isolated_sys_path, python._pth)

  # ._pth files don't work correctly on non-Windows prior to Python 3.11.
  # For this reason we also modify the shebang on non-Windows platforms.
  # As an alternative, we could in `guild()` call `python -I -m guild.main_bootstrap`
  # instead of the console_script entry point `guild`.
  if (!is_windows()) {
    guild <- file.path(venv, "bin/guild")
    shebang_txt <- readLines(guild)
    if ('-e' %in% guildai) {
      shebang <- c(
        "#!/bin/sh",
        sprintf("'''exec' \"%s\" -I -Xfrozen_modules=off \"$0\" \"$@\"",
                file.path(venv, "bin/python")),
        "' '''"
      )
      shebang_txt <-
        c(shebang,
          shebang_txt[-(if (grepl("python", shebang_txt[1])) 1 else 1:3)])

    } else if (grepl("/python[0-9.]*$", shebang_txt[1]))
      shebang_txt[1] <- paste(shebang_txt[1], "-I")
    else if (grepl("exec", shebang_txt[2])) {
      # on macOS, the console_script entry point looks like this:
      ## #!/bin/sh
      ## '''exec' "/Users/tomasz/Library/Application Support/r-guildai/bin/python" "$0" "$@"
      ## ' '''
      ## # -*- coding: utf-8 -*-
      ## import re
      ## ...
      x <- shebang_txt[2]
      if (endsWith(x, ' "$0" "$@"')) {
        x <- str_drop_suffix(x, ' "$0" "$@"')
        x <- paste(x, "-I", '"$0" "$@"')
        shebang_txt[2] <- x
      }
    }
    writeLines(shebang_txt, guild)
  }

  message("Finished installing guildai!")

  guild_exe <- if (is_windows())
    file.path(venv, "Scripts", "guild.exe", fsep = "\\") else
    file.path(venv, "bin", "guild")

  invisible(guild_exe)
  }

# install_guild(c("-e", "~/guild/guildai"), reticulate::install_python("3.11:latest"))
# #!/bin/sh
# '''exec' "/home/tomasz/.local/share/r-guildai/bin/python" -I -Xfrozen_modules=off "$0" "$@"
# ' '''
# # -*- coding: utf-8 -*-
# import re

# install_guild(
#   guildai = "https://api.github.com/repos/guildai/guildai/tarball/HEAD",
#   python = reticulate::install_python("3.11:latest"))
# install_guild(c("-e", normalizePath("~/guild/guildai")), reticulate::install_python())
# install_guild(c(normalizePath("~/guild/guildai")), reticulate::install_python())

find_guild <- function() {
  if (file.exists(guild <- Sys.which("guild")))
    return(normalizePath(as.vector(guild)))
  find_r_guildai_guild()
}

find_r_guildai_guild <- function() {
  if (is_windows())
    if (file.exists(guild <-
                    file.path(rappdirs::user_data_dir("r-guildai", NULL),
                              "Scripts", "guild.exe")))
      return(normalizePath(as.vector(guild)))
  if (file.exists(guild <-
                  file.path(rappdirs::user_data_dir("r-guildai", NULL), "bin", "guild")))
    return(normalizePath(as.vector(guild)))
  install_guild()
}


#' Install guild for usage in the Terminal
#'
#' This function makes available the `guild` executable installed by
#' `install_guild()` for usage in the Terminal.
#'
#' Note that the guild executable installed by the R function
#' `install_guild()` is not able to run python operations. To run python
#' operations with guild, you must install guild into the target python
#' installation with `pip install guildai`, and ensure that the desired guild
#' executable is on the `PATH`.
#'
#' @param dest Directory where to place the `guild` executable. This should
#'   be a location on the `PATH`.
#' @param completions Whether to also install shell completion helpers.
#'
#' @return path to the installed guild executable, invisibly.
#' @export
install_guild_cli <-
function(dest = "~/bin",
         completions = basename(Sys.getenv("SHELL")) %in% c("bash", "zsh", "fish")) {
  if(!dir.exists(dest))
    dir.create(dest)

  guild_exe <- find_r_guildai_guild()
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

  if (nzchar(Sys.which("guild")) &&
      normalizePath(Sys.which("guild")) != normalizePath(path.expand(link))) {
    warning("A different 'guild' executable is first on the PATH: ",
            "'", normalizePath(Sys.which("guild")), "'")
  }

  invisible(link)
}
