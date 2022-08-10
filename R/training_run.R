


do_guild_run <- function(path_to_r_script, flags_dest = "globalenv",
                         flags = parse_command_line(commandArgs(TRUE))) {
    exprs <- parse(path_to_r_script)
    if (flags_dest == "globalenv") {
      exprs <- inject_global_flag_values(exprs, flags)
    } else
      stop("only globalenv flags interface implemented")

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

    e[[3L]] <- flags[[name]]
    exprs[[i]] <- e
    flags[[name]] <- NULL

    if (!length(flags))
      break


  }
  exprs
}


parse_command_line <- function(arguments) {

  values <- list()

  i <- 0; n <- length(arguments)
  while (i < n) {
    i <- i + 1
    argument <- arguments[[i]]

    # skip any command line arguments without a '--' prefix
    if (!grepl("^--", argument))
      next

    # terminate if we see "--args" (implies passthrough args)
    if (grepl("^--args$", argument))
      break

    # check to see if an '=' was specified for this argument
    equals_idx <- regexpr("=", argument)
    if (identical(c(equals_idx), -1L)) {
      # no '='; the next argument is the value for this key
      key <- substring(argument, 3)
      val <- arguments[[i + 1]]
      i <- i + 1
    } else {
      # found a '='; the next argument is all the text following
      # that character
      key <- substring(argument, 3, equals_idx - 1)
      val <- substring(argument, equals_idx + 1)
    }

    # convert '-' to '_' in key
    key <- gsub("-", "_", key)

    # update our map of argument values
    values[[key]] <- yaml::yaml.load(val)
  }

  values

}
