



#' list guild runs
#'
#' @return a dataframe of runs
#' @export
ls_runs <- function() {
  x <- guild("runs", "--json", stdout = TRUE)
  x <- jsonlite::parse_json(x, simplifyVector = TRUE)
  tibble::as_tibble(x)
}


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
#' @return NULL, invisibly. This function is called for its side effect.
#' @export
guild_run <- function(opspec = "train.R", flags = NULL, wait = TRUE, echo = wait) {
  if (is.data.frame(flags)) {
    for (r in seq_len(nrow(flags)))
      guild_run(opspec, unclass(flags[r, ]), echo = echo, wait = wait)
    return(invisible())
    # TODO: writeout flags to tempfile csv/json/yaml, supply to
    # guild call like: `guild run '@/path/to/tmpdir/tmpfile.json`
  }

  if (!is.null(names(flags))) {
    # browser()
    # flags <- lapply(flags, function(x) if(length(x) > 1))
    flags <- lapply(flags, function(x)
      if(is.character(x)) x else encode_yaml(x))
    flags <- sprintf("%s=%s", names(flags), unname(flags))
  }

  cl <- quote(guild("run", "--yes", opspec, flags, wait = wait))
  if (!echo)
    cl$stdout <- cl$stderr <- FALSE
  eval(cl)
  invisible()
}



guild_view <- function() {
  # use processx here?
  guild("view", wait = FALSE)
}
