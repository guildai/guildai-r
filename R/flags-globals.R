

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

peek_r_script_guild_info <- function(r_script_path) {

  text <- readLines(r_script_path)
  is_anno <- startsWith(trimws(text, "left"), "#|")

  frontmatter <- NULL
  if(is_anno[1])
    frontmatter <- parse_yaml_hints(text[seq_len(which.min(is_anno)-1L)])
  else if(startsWith(text[1], "#!/") && is_anno[2])
    # allow frontmatter to start on 2nd line if first line is a shebang
    frontmatter <- parse_yaml_hints(text[seq_len(which.min(is_anno)-1L)][-1])

  params <- frontmatter$flags
  if(is.null(params))
    params <- infer_global_params(text, is_anno)

  cat(unclass(jsonlite::toJSON(
    list(frontmatter = frontmatter,
         "global-flags" = params),
    auto_unbox = TRUE,
    digits = 16, pretty = interactive())))

  invisible()
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

    line_num <- utils::getSrcLocation(exprs[i], "line")

    param <- list(name = name,
                  default = default,
                  type = switch(typeof(default),
                                "double" = "float",
                                "logical" = "bool",
                                "character" = "string",
                                "integer" = "int",
                                "complex" = "complex"),
                  line_num = line_num)

    # look up and down for adjacent hints about this flag
    if (is_anno[line_num - 1L]) {
      anno_start <- anno_end <- line_num - 1L
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
  x <- rlang::dots_list(..., .named = TRUE)
  print(jsonlite::toJSON(x, auto_unbox = TRUE, digits = 16))
}
