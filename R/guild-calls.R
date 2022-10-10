

#' list guild runs
#'
#' @param ... additional arguments passed to `guild api runs`. Try
#'   `"--help"` to see options.
#'
#' @return a dataframe of runs
#' @export
#' @importFrom jsonlite parse_json
ls_runs <- function(...) {
  if ("--help" %in% c(...))
    return(guild("api", "runs", "--help"))
  # --json option always shows all runs
  x <- guild("api", "runs", ..., stdout = TRUE)
  df <- parse_json(x, simplifyVector = TRUE)

  # TODO: guild should return something that's tz aware,
  #   This approach the best we can do is assume the dt string is
  #   in the user locale
  for(dt_name in c("started", "stopped"))
    df[[dt_name]] <- as.POSIXct(df[[dt_name]],
                                  format = "%Y-%m-%d %H:%M:%OS")
  df$time <- as.difftime(df$time, units = "secs")

  if("tibble" %in% loadedNamespaces())
    df <- tibble::as_tibble(df)

  df$comments <- lapply(df$comments, as.character)

  df
}




latest_run <- function() {

}

# TODO: support for globals injection of `!expr foo` flags

#' Launch a guild run
#'
#' @param opspec typically path to an R script, but could be an scalar
#'   string that guild recognizes as a valid opspec.
#' @param flags
#'
#'   - a named list or vector like `c(noise = .3, dropout = .4)`
#'
#'   - a scalar string like `"noise=.3 dropout=.4"`
#'
#'   - a dataframe of flags for a batch of runs
#'
#' @param wait whether to wait for the run to finish
#' @param echo whether output from the run is shown in the current R
#'   console. Note, this has no effect on whether expressions are echoed in
#'   the guild run stdout.
#'
#' @param ... passed through to [base::system2]
#'
#' @return NULL, invisibly. This function is called for its side effect.
#' @export
guild_run <- function(opspec = "train.R", flags = NULL, wait = TRUE,
                      label = NULL,
                      tags = NULL,
                      echo = wait, ...) {
  if (is.data.frame(flags)) {
    cl <- match.call()
    for (r in seq_len(nrow(flags))) {
      cl$flags <- unclass(flags[r, ])
      eval(cl)
    }
    return(invisible())
    # TODO: writeout flags to tempfile csv/json/yaml, supply to
    # guild call like: `guild run '@/path/to/tmpdir/tmpfile.json`
  }

  if (!is.null(names(flags))) {
    # A scalar string for flags is passed through
    # otherwise, do some prep to build the command line arg
    flags <- lapply(flags, function(f) {
      x <- vapply(f,
                  function(fv) if(is.character(fv)) fv else encode_yaml(fv),
                  "", USE.NAMES = FALSE)
      if(length(x) > 1)
        x <- sprintf("[%s]", paste0(x, collapse = ","))
      x
    })
    flags <- sprintf("%s=%s", names(flags), unname(flags))
  }

  args <- c("--yes")
  if(is.character(label))
    append(args) <- c("--label", label)
  if(is.character(tags))
    append(args) <- as.vector(rbind("--tag", tags))

  cl <- call("guild", "run", args,
             opspec, flags, wait = wait,
             quote(...))
  if (!echo)
    cl$stdout <- cl$stderr <- FALSE
  if(Sys.getenv("DEBUGR") == "1")
    message("R> ", deparse1(cl))
  eval(cl)
  invisible()
}



guild_view <- function() {
  # TODO: use processx here?
  guild("view", wait = FALSE)
}

