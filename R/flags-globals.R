

#', #+, #- roxygen, knitr::spin
#| quarto, rmarkdown
#* plumbr
#| guild ?
#: ?    #= #+ #^ #) #} #] #!



# r_script_path <- "tests/resources/example-r-script.R"

parse_yaml_anno <- function(x) {
  stopifnot(startsWith(x, "#|"))
  x <- substr(x, 4L, .Machine$integer.max)
  x <- yaml::yaml.load(x)
  x
}

is_hashpipe <- function(x) startsWith(x, "#|")

#' @importFrom magrittr %<>%
#' @importFrom rlang %||%
#' @importFrom utils modifyList
emit_r_script_guild_data <- function(r_script_path) {

  text <- readLines(r_script_path)
  is_anno <- startsWith(trimws(text, "left"), "#|")

  # defaults
  data <- list(
    name = r_script_path,
    exec = sprintf(
      r"(Rscript -e 'guildai:::do_guild_run("%s")' ${flag_args})",
      r_script_path),
    `flags-dest` = "globals",
    sourcecode = list(
      dest = ".",
      select = list(
        list(exclude = list(dir = "renv")),
        list(include = list(text = list("renv.lock", ".Rprofile", ".Renviron",
                                   "**.[rR]")))
      )))
  # "flags-dest": 'globals',
  # "flags": script_data['global-flags'],
  # "output-scalars": self.output_scalars,
  # "objective": self.objective,
  # "plugins": self.plugins,

  update_data <- function(x) {
    data <<- modifyList(data, x)
  }


  if (is_anno[1]) {
    update_data(parse_yaml_hints(text[seq_len(which.min(is_anno) - 1L)]))
  } else if (startsWith(text[1], "#!/") && is_anno[2])
    # allow frontmatter to start on 2nd line if first line is a shebang
    update_data(parse_yaml_hints(text[seq_len(which.min(is_anno) - 1L)][-1]))


  if(data[["flags-dest"]] == "globals") {
    flags <- infer_global_params(text, is_anno)
    if(!is.null(data[["flags"]])) {
      flags <- modifyList(flags, data[["flags"]])
    }
    data <- modifyList(list(flags = flags), data)
    # frontmatter supplied data takes precedence.
  }

  # op_name <- basename(r_script_path) |> tools::file_path_sans_ext()

  # emit(list("model" = dirname(r_script_path),
  #           operations = list(data)
  emit(data)
}


emit <- function(x) {
  out <- yaml::as.yaml(
    x, precision = 16L, indent.mapping.sequence = TRUE)
  cat(out)
  invisible(out)
}

infer_global_params <- function(text, is_anno = startsWith(trimws(text, "left"), "#|")) {

  exprs <- parse(text = text, keep.source = TRUE)

  params <- list()
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
                                "logical" = "bool",
                                "character" = "string",
                                "integer" = "int",
                                "complex" = "complex")
                  # , lineno = lineno
                  )


    lineno <- utils::getSrcLocation(exprs[i], "line")
    # look up and down for adjacent hints about this flag
    if (is_anno[lineno - 1L]) {
      anno_start <- anno_end <- lineno - 1L
      while (is_anno[anno_start - 1L])
        anno_start <- anno_start - 1L

      anno <- parse_yaml_anno(text[anno_start:anno_end])
      param <- c(param, anno)
    }

    params[[name]] <- param

  }

  params

}


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

do_guild_run <-
  function(file = "train.R",
           params = parse_command_line(commandArgs(TRUE))) {
    exprs <- parse(file, keep.source = TRUE)
    exprs <- inject_global_param_values(exprs, params)
    source(exprs = exprs)
    invisible()
  }


inject_global_param_values <- function(exprs, params) {
  if (!length(params))
    return(exprs)

  for (i in seq_along(exprs)) {
    e <- exprs[[i]]

    if (!is.call(e))
      next

    op <- e[[1L]]
    if (op != quote(`=`) && op != quote(`<-`))
      next

    if (typeof(e[[2L]]) != "symbol")
      next

    name <- as.character(e[[2L]])
    if (!name %in% names(params))
      next

    if(identical(params[[name]], e[[3L]])) { # new value same as default
      params[[name]] <- NULL
      next
    }

    old_e <- e

    # `e` is an assignment expression with a flag symbol on
    # the left hand side
    e[[3L]] <- params[[name]] # replace the value in node
    exprs[[i]] <- e           # update exprs w/ new node
    params[[name]] <- NULL    # null out flag value; each flag is injected only once

    message(sprintf("Replacing expression '%s' on line %i with '%s'",
                    deparse1(old_e), utils::getSrcLocation(exprs[i], "line"),
                    deparse1(e)))

    if (!length(params))
      break
  }

  if(length(params))
    warning("Unused params received but not injected: ", unlist(params))

  exprs
}

#' @export
guild_log <- function(...) {
  emit(rlang::dots_list(..., .named = TRUE))
}
