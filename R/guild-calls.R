


#' @export
ls_runs <- function() {
  x <- guild("runs", "--json", stdout = TRUE)
  x <- jsonlite::parse_json(x, simplifyVector = TRUE)
  tibble::as_tibble(x)
}

#' @export
guild_run <- function(file = "train.R", flags = NULL, echo = TRUE) {
  # echo not supported yet, see "Q's for Garrett"
  if(is.data.frame(flags)) {
    for(r in seq_len(nrow(flags)))
      guild_run(file, unclass(flags[r,]), echo)
    return()
    # TODO: writeout flags to tempfile csv/json/yaml, supply to
    # guild call like: `guild run '@/path/to/tmpdir/tmpfile.json`
  }

  if(!is.null(flags))
    flags <- shQuote(sprintf("%s=%s", names(flags), as.character(flags)))
  guild("run", "--yes", file, flags) #if(echo) "--echo",
}




guild_view <- function() {
  # use processx here?
  guild("view", wait = FALSE)
}
