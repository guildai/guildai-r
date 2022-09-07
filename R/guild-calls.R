


#' @export
ls_runs <- function() {
  x <- guild("runs", "--json", stdout = TRUE)
  x <- jsonlite::parse_json(x, simplifyVector = TRUE)
  tibble::as_tibble(x)
}

#' @export
guild_run <- function(file = "train.R", flags = NULL, echo = TRUE) {
  if(!is.null(flags))
    flags <- shQuote(sprintf("%s=%s", names(flags), as.character(flags)))
  guild("run", "--yes",  file, flags) #if(echo) "--echo",
}


guild_view <- function() {
  guild("view", wait = FALSE)
}
