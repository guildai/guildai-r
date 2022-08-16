

#', #+, #- roxygen, knitr::spin
#| quarto, rmarkdown
#* plumbr
#| guild
#: ?    #= #+ #^ #) #} #]

# r_script_path <- "tests/resources/example-r-script.R"

parse_yaml_hints <- function(x) {
  stopifnot(startsWith(x, "#|"))
  x <- substr(x, 4L, .Machine$integer.max)
  x <- yaml::yaml.load(x)
  x
}

is_hashpipe <- function(x) startsWith(x, "#|")


peek_r_script_guild_info <- function(r_script_path) {

  text <- readLines(r_script_path)
  is_hint <- startsWith(text, "#|")

  frontmatter <- NULL
  if(is_hint[1])
    frontmatter <- parse_yaml_hints(text[seq_len(which.min(is_hint)-1L)])
  else if(startsWith(text[1], "#!/") && is_hint[2])
    # allow frontmatter to start on 2nd line if first line is a shebang
    frontmatter <- parse_yaml_hints(text[seq_len(which.min(is_hint)-1L)][-1])

  exprs <- parse(text = text, keep.source = TRUE)

  flags <- list()
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

    if(name %in% names(flags))
      next

    default <- e[[3L]]
    if (is.call(default)) {
      # allow simple exprs of basic fns and literals
      # convenient to specify some flags as simple math, e.g, power of 2
      if (!all(all.names(default) %in%
               c("+", "-", "*", "/", "^", "(")))
        next
      default <- eval(default, envir = baseenv())
    }

    stopifnot(
      typeof(default) %in%
        c("double", "integer", "character", "logical", "complex"),
      identical(length(default), 1L)
    )


    line_num <- getSrcLocation(exprs[i], "line")

    flag <- list(name = name,
                 default = default,
                 type = switch(typeof(default),
                               "double" = "float",
                               "logical" = "bool",
                               "character" = "string",
                               "integer" = "int",
                               "complex" = "complex"),
                 "line-num" = line_num)

    # look up and down for adjacent hints about this flag
    if (is_hint[line_num - 1L]) {
      hints_start <- hints_end <- line_num - 1L
      while (is_hint[hints_start - 1L])
        hints_start <- hints_start - 1L

      hints <- parse_yaml_hints(text[hints_start:hints_end])
      flag <- c(flag, hints)
    }

    flags[[name]] <- flag

  }


  cat(unclass(jsonlite::toJSON(
    list(frontmatter = frontmatter,
         "global-flags" = flags),
    auto_unbox = TRUE,
    digits = 16, pretty = interactive())))

  invisible()
}





run_with_global_flags <-
  function(file = "train.R",
           flags = parse_command_line(commandArgs(TRUE))) {
    exprs <- parse(file, keep.source = TRUE)
    exprs <- inject_global_flag_values(exprs, flags)
    source(exprs = exprs)
    invisible()
  }


inject_global_flag_values <- function(exprs, flags) {
  if (!length(flags))
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
    if (!name %in% names(flags))
      next


    if(identical(flags[[name]], e[[3L]])) # new value same as default
      next

    message(sprintf("Replacing flag '%s' on line %i with %s",
                    name, getSrcLocation(exprs[i], "line"), flags[[name]]))
    # `e` is an assignment expression with a flag symbol on
    # the left hand side
    e[[3L]] <- flags[[name]] # replace the value in node
    exprs[[i]] <- e          # update exprs w/ new node
    flags[[name]] <- NULL    # null out flag value; each flag is injected only once

    if (!length(flags))
      break
  }

  if(length(flags))
    warning("Unused flags received but not injected: ", unlist(flags))

  exprs
}
