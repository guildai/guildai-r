



peek_r_script_global_flags <- function(r_script_path) {

  exprs <- parse(r_script_path, keep.source = TRUE)

  flags <- list()
  for(e in exprs) {

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

    flags[[name]] <- default
  }

  cat(unclass(jsonlite::toJSON(flags, auto_unbox = TRUE, digits = NA)))

  invisible()
}





run_with_global_flags <-
  function(file = "train.R",
           flags = parse_command_line(commandArgs(TRUE))) {
    exprs <- parse(file)
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
