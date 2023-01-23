


#' @importFrom rlang %||% is_string
#' @importFrom utils modifyList
emit_r_script_guild_data <- function(r_script_path = commandArgs(TRUE)[1])
  print.yaml(r_script_guild_data(r_script_path))

# TODO: provide a user api for programmatically inspecting flags for
# a given opspec.
# It most likely should be through this interface:
#   parse_json(guild("api help-op train.R", stdout=T))
# but that api endpoint needs to be patched first.

r_script_guild_data <- function(r_script_path) {

  r_script_path <- normalizePath(r_script_path, winslash = "/")
  # we need to convert this to a relative path to the guild cwd/project root.
  # for now we assume that the sourcecode.root == getwd() and sourcecode.dest == "."
  # TODO: make sure the deparsed call in the exec string contains a relative path
  # to the script in the RUN_DIR, if user supplied non-default 'root' or 'dest' or
  # launched from not the project dir.
  cwd <- normalizePath(getwd(), winslash = "/")
  if(!startsWith(r_script_path, cwd))
    stop("the script path must be under the current working directory")
  r_script_path <- paste0(".", str_drop_prefix(r_script_path, cwd))
  r_script_path <- str_drop_prefix(r_script_path, "./")

  text <- readLines(r_script_path)
  # handle case of empty file
  if(!length(text)) text <- ""

  # op default data
  data <- yaml(
    "name" = r_script_path,
    "flags-dest" = r_script_path,
    "echo" = TRUE
  )

  if (is_windows()) {
    # need / as the path.sep in file paths that will be passed through
    # the cmdline. guild parses the cmdline using posix conventions,
    # even on windows.
    for (potential_path in list("name", "flags-dest",
                                c("sourcecode", "root"),
                                c("sourcecode", "dest"))) {
      path <- tryCatch(data[[potential_path]], error = function(e) NULL)
      if (!is_string(path))
        next

      data[[potential_path]] <-
        gsub("\\", "/", data[[potential_path]], fixed = TRUE)
    }
  }

  is_anno <- startsWith(trimws(text, "left"), "#|")
  if (is_anno[1] || startsWith(text[1], "#!/") && is_anno[2]) {
    # script has frontmatter
    # frontmatter can start on 2nd line if first line is a shebang

    anno_start <- which.max(is_anno)
    anno_end <- which.min(c(TRUE, is_anno[-1L])) -1L

    frontmatter <- parse_yaml_anno(text[anno_start:anno_end])
    # update default data w/ user values
    data <- as_yaml(config::merge(data, frontmatter))
  }

  # user supplied sourcecode select rules get appended to the default rules
  # We do it here because config::merge() would overwrite
  # data$sourcecode$select otherwise

  # directories that start with a . prefix like ".Rproj.user" are
  # automatically excluded by default. But not .files
  if (file.exists(".Rhistory")) {
    prepend(data$sourcecode$select) <-
      list(list(exclude = list(text = ".Rhistory")))
  }

  # "logs" is the default write location for
  # tfevents::log_event() and keras::callback_tensorboard()
  if (dir.exists("logs")) {
    prepend(data$sourcecode$select) <-
      list(list(exclude = list(dir = "logs")))
  }

  if(dir.exists("renv") && file.exists("renv.lock")) {
    prepend(data$sourcecode$select) <-
      list(list(exclude = list(dir = "renv")))
    prepend(data$requires) <-
      list(list(file = "renv", "target-type" = "link"))
  }

  # if user supplied flags in frontmatter:
  #   use that directly, don't do any inference
  # else:
  #   walk script ast looking for symbols assigned
  #   at top level values of length-1 atomic literals

  flags_dest <- data$`flags-dest`
  if (is.null(data$flags)) {

    if (is_r_file(flags_dest)) {
      data$flags <- infer_global_params(text, is_anno)
    } else if (is_yml_file(flags_dest)) {
      # TODO(gs/tk): this file read should be done by guild core
      data$flags <- read_yaml(str_drop_prefix(flags_dest, "config:"))
    }
  }


  # intercept user supplied `echo` here
  echo <- data$echo %||% TRUE
  data$echo <- NULL

  cl <- call("do_guild_run", r_script_path)

  # don't pass through flags_dest to do_guild_run(),
  # because guild core will materialize the yml file.
  if(startsWith(flags_dest, "config:"))
    flags_dest <- NULL

  if(!identical(flags_dest, r_script_path))
    cl["flags_dest"] <- list(flags_dest) # preserve NULL

  if(!isTRUE(echo))
    cl$echo <- echo
  cl <- call(":::", quote(guildai), cl)

  data$exec <- sprintf('%s -e %s',
                       shQuote(rscript_exe(), type = "sh"),
                       shQuote(deparse1(cl), type = "sh"))

  data
}


rscript_exe <- function() {
  file.path(R.home("bin"),
            if (is_windows()) "Rscript.exe" else "Rscript")
}


infer_global_params <- function(text, is_anno = startsWith(trimws(text, "left"), "#|")) {

  exprs <- parse(text = text, keep.source = TRUE)

  # 0-length names to force a yaml mapping if no flags.
  params <- structure(list(), names = character())

  for(i in seq_along(exprs)) {
    e <- exprs[[i]]

    if(!is.call(e))
      next

    op <- e[[1L]]
    if(op != quote(`=`) && op != quote(`<-`))
      next

    if(typeof(e[[2L]]) != "symbol")
      next

    name <- as.character(e[[2L]])

    if(name %in% names(params))
      next

    default <- e[[3L]]
    if (is.call(default)) {
      # constants like "+1" and "-1", "1+1i", etc.
      # parsed as calls of "+" and "-"
      if (!all(all.names(default) %in% c("+", "-")))
        next
      default <- eval(default, envir = baseenv())
    }

    if(!typeof(default) %in% c("double", "integer", "character", "logical", "complex") ||
       !identical(length(default), 1L))
      next

    param <- list(default = default,
                  type = switch(typeof(default),
                                "double" = "number",
                                "logical" = "boolean",
                                "character" = "string",
                                "integer" = "int",
                                "complex" = "string"))
    # yaml has no native support for complex

    lineno <- utils::getSrcLocation(exprs[i], "line")
    # look for adjacent anno hints about this flag
    if (isTRUE(is_anno[lineno - 1L])) {
      anno_start <- anno_end <- lineno - 1L
      while (isTRUE(is_anno[anno_start - 1L]))
        subtract(anno_start) <- 1L
      # Make sure we aren't confusing op config for flag config
      # i.e., we didn't pick up frontmatter anno and treat it as flag anno
      # if user did something like:
      #1  #| echo: false
      #2  y <- 0
      if (!(anno_start == 1L ||
            (anno_start == 2L && startsWith(text, "#!/")))) {
        anno <- parse_yaml_anno(text[anno_start:anno_end])
        append(param) <- anno
      }
    }

    params[[name]] <- param

  }

  params

}

# TODO (maybe): add a yaml option 'save' for post-run save.image():
##   save: true, false, or list of symbols like [x, y, model]
##   determines if `save.image()` is called at the end of the R run
##   or passed --save to the command line opts
