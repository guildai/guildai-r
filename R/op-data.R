

#  #', #+, #- roxygen, knitr::spin
#| quarto, rmarkdown
#* plumbr
#| guild ?
#: ?    #= #+ #^ #) #} #] #!




# r_script_path <- "tests/resources/example-r-script.R"

parse_yaml_anno <- function(x) {
  stopifnot(startsWith(x, "#|"))
  x <- substr(x, 4L, .Machine$integer.max)
  x <- parse_yaml(x)
  x
}


#' @importFrom magrittr %<>%
#' @importFrom rlang %||%
#' @importFrom utils modifyList

r_script_path <- "../sample-proj/test2.R"


emit_r_script_guild_data <- function(r_script_path)
  print.yaml(guild_data(r_script_path),
             c("", "guild-op-data.yml"))


guild_data <- function(r_script_path) {

  text <- readLines(r_script_path)
  is_anno <- startsWith(trimws(text, "left"), "#|")

  data <- read_yaml(system.file("default-rscript-guild.yml",
                                package = "guildai"))

  update_data <- function(x) {
    data <<- as_yaml(config::merge(data, x))
    # data <<- merge_guild_data(data, x)
    data
  }

  update_data(list(
    name = r_script_path,
    exec = sprintf(
      r"(Rscript -e 'guildai:::do_guild_run("%s")' ${flag_args})",
      r_script_path
    )
  ))

  frontmatter <-  if (is_anno[1]) {
    parse_yaml_anno(text[seq_len(which.min(is_anno) - 1L)])
  } else if (startsWith(text[1], "#!/") && is_anno[2])
    # allow frontmatter to start on 2nd line if first line is a shebang
    parse_yaml_anno(text[seq_len(which.min(is_anno) - 1L)][-1])

  update_data(frontmatter)


  if(data[["flags-dest"]] == "globals") {
    # walk script ast looking for constants defined at top level
    flags <- infer_global_params(text, is_anno)

    # if user supplied flags in anno frontmatter, merge w/ the found flags
    if(!is.null(data[["flags"]]))
      flags <- modifyList(flags, data[["flags"]])

    # merge flags into output data, frontmatter supplied data takes precedence.
    data <- modifyList(list(flags = flags), data)
  }

  data
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
                                "complex" = "complex"))


    lineno <- utils::getSrcLocation(exprs[i], "line")
    # look for adjacent anno hints about this flag
    if (is_anno[lineno - 1L]) {
      anno_start <- anno_end <- lineno - 1L
      while (is_anno[anno_start - 1L])
        subtract(anno_start) <- 1L

      anno <- parse_yaml_anno(text[anno_start:anno_end])
      append(param) <- anno
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



# - does guild infer an incremented 'step' when encountering a duplicate key?
# - Can we have a nicer run_dir uuid ala timestamped sortable like tfruns::unique_run_dir()?
# - Can we change default GUILD_HOME to be "./.guild"
# - Can we change default runs dir to be "./runs" for a project?
# - instead of 'sourcecode: dest: '.'', what if we kept the current dest, but
#   symlinked all the sourcecode files from the rundir pointing to the .sourcecode dest?
#   (would need to be a change in guild)
# - related: sourcecode files should be copied with `chmod -w` settings
# - can a plugin define additional guild options, like --echo or --no-echo?
