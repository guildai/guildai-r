


parse_command_line <- function(arguments) {

  values <- list()

  i <- 0L; n <- length(arguments)
  while (i < n) {
    i <- i + 1L
    argument <- arguments[[i]]

    # skip any command line arguments without a '--' prefix
    if (!grepl("^--", argument))
      next

    # terminate if we see "--args" (implies passthrough args)
    if (grepl("^--args$", argument))
      break

    # check to see if an '=' was specified for this argument
    equals_idx <- regexpr("=", argument, fixed = TRUE)
    if (identical(c(equals_idx), -1L)) {
      # no '='; the next argument is the value for this key
      key <- substring(argument, 3)
      i <- i + 1L
      val <- arguments[[i]]
    } else {
      # found a '='; the next argument is all the text following
      # that character
      key <- substring(argument, 3L, equals_idx - 1L)
      val <- substring(argument, equals_idx + 1L)
    }

    # convert '-' to '_' in key
    key <- gsub("-", "_", key)

    if(nzchar(val))
      val <- 'false'
    # update our map of argument values
    values[[key]] <- yaml::yaml.load(val)
  }

  values

}
