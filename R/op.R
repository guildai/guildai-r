


#' @importFrom rlang %||%
#' @importFrom utils modifyList
emit_r_script_guild_data <- function(r_script_path = commandArgs(TRUE)[1]) {
  print.yaml(r_script_guild_data(r_script_path),
             c("", if(Sys.getenv("DEBUGR") == "1")
               "emitted-script-guild-op-data.yml"))
}


r_script_guild_data <- function(r_script_path) {

  r_script_path <- gsub("\\", "/", r_script_path, fixed = TRUE)
  r_script_path <- str_drop_prefix(r_script_path, "./")

  text <- readLines(r_script_path)
  # handle case of empty file
  if(!length(text))
    text <- ""
  is_anno <- startsWith(trimws(text, "left"), "#|")

  data <- yaml(
    "flags-dest" = r_script_path,
    "name" = r_script_path,
    "sourcecode" =
      list(dest = ".",
           select = list(
             list(exclude =
                    list(dir = list("renv", "env", "venv", "logs"))))),
    "echo" = TRUE,
    "pip-freeze" = FALSE
  )

  update_data <- function(x)
    invisible(data <<- as_yaml(config::merge(data, x)))

  frontmatter <-
    if (is_anno[1] || startsWith(text[1], "#!/") && is_anno[2]) {
    # allow frontmatter to start on 2nd line if first line is a shebang

    anno_start <- which.max(is_anno)
    anno_end <- which.min(c(TRUE, is_anno[-1L])) -1L

    parse_yaml_anno(text[anno_start:anno_end])
  }

  sourcecode_select <- frontmatter$sourcecode$select
  update_data(frontmatter)

  # user supplied sourcecode select rules get appended to the default rules
  # config::merge overwrites data$sourcecode$select otherwise
  append(data$sourcecode$select) <- sourcecode_select

  # if user supplied flags in frontmatter:
  #   use that directly, don't do any inference
  # else:
  #   walk script ast looking for symbols assigned
  #   at top level values of length-1 atomic literals

  if (is.null(data$flags)) {
    flags_dest <- data$`flags-dest`

    if (is_r_file(flags_dest)) {
      data$flags <- infer_global_params(text, is_anno)

    } else if (is_yml_file(flags_dest)) {
      # TODO: this file read should be done by guild core
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

  data$exec <- sprintf("%s -e %s",
                       rscript_bin(),
                       shQuote(deparse1(cl)))

  data
}


rscript_bin <- function() {
  # TODO: do we need arch in the file path on windows?
  file.path(R.home("bin"),
            if(is_windows()) "Rscript.exe" else "Rscript")
}


infer_global_params <- function(text, is_anno = startsWith(trimws(text, "left"), "#|")) {

  # TODO: figure out how to present a nice error message in case of parse errors
  # from python plugin / guild
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
      # allow simple exprs of basic fns and literals
      # convenient to specify some params as simple math, e.g, power of 2
      if (!all(all.names(default) %in% SIMPLE_MATH_OPS))
        next
      default <- eval(default, envir = baseenv())
    }

    if(!typeof(default) %in% c("double", "integer", "character", "logical", "complex") ||
       !identical(length(default), 1L))
      next


    param <- list(default = default,
                  type = switch(typeof(default),
                                "double" = "float",
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
      # TODO: add test if lineno==1L to ensure no error
      # TODO: expand check to make sure we don't falsly pickup frontmatter as flag anno.
      #1  #| echo: false
      #2  y <- 0

      anno <- parse_yaml_anno(text[anno_start:anno_end])
      append(param) <- anno
    }

    params[[name]] <- param

  }

  params

}

## TODO: guild bug in the way it writes out flags
## If `y` is a flag, guild writes out this to attrs, which is not valid yaml:
## y: 0.0
## It should be
## 'y': 0.0
## Otherwise, this get's parsed into R as
## yaml::yaml.load("y: 0.0") |> dput()
## list(`TRUE` = 0)

## TODO: a way to declare required packages that can't otherwise be easily infered?
## ? #| requires: {packages: [glmnet]}  or similar

## TODO:
## if user has flags-dest: file:foo.yml
## then give a very clean exec cmd:
##   R (if echo:false --no-echo) --file=file.R
##
## Q: do we default to --no-restore?
##   Rscript expands to:
##   /opt/R/4.2.1/lib/R/bin/exec/R --no-echo --no-restore --file=train-basic.R

## add a yaml option:
##   save: true, false, or list of symbols like [x, y, model]
##   determines if `save.image()` is called at the end of the R run
##   or passed --save to the command line opts

# as_flag_spec <- function(x) {
#
#
#   param <- list(default = default,
#                 type = switch(typeof(default),
#                               "double" = "float",
#                               "logical" = "bool",
#                               "character" = "string",
#                               "integer" = "int",
#                               "complex" = "complex"))
#
#
#
# }


SIMPLE_MATH_OPS <-
  c( ## Math
    "abs", "sign", "sqrt", "floor", "ceiling", "trunc", "round",
    "signif", "exp", "log", "expm1", "log1p", "cos", "sin", "tan",
    "cospi", "sinpi", "tanpi", "acos", "asin", "atan", "cosh", "sinh",
    "tanh", "acosh", "asinh", "atanh", "lgamma", "gamma", "digamma",
    "trigamma", "cumsum", "cumprod", "cummax", "cummin",

    ## Ops
    "+", "-", "*", "/", "^", "%%", "%/%",

    ## Summary
    "sum", "prod", "min", "max"
  )


## Q's for Garrett
# - does guild infer an incremented 'step' when encountering a duplicate key?
# - Can we have a nicer run_dir uuid ala timestamped sortable like tfruns::unique_run_dir()?
# - Can we change default GUILD_HOME to be "./.guild"
# - Can we change default runs dir to be "./runs" for a project?
# - instead of 'sourcecode: dest: '.'', what if we kept the current dest, but
#   symlinked all the sourcecode files from the rundir pointing to the .sourcecode dest?
#   (would need to be a change in guild)
# - related: sourcecode files should be copied with `chmod -w` settings
# - can a plugin define additional guild options, like --echo or --no-echo?
# - Should guild provide a keras callback that writes additional
#   guild run metadata, like model summary, metrics, etc.?


