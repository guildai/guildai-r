

#' list guild runs
#'
#' @param full Whether to include all the available run info.
#'
#' @param ... additional arguments passed to `guild api runs`. Try
#'   `"--help"` to see options.
#'
#' @return a dataframe of runs
#' @export
#' @importFrom jsonlite parse_json
runs_info <- function(..., full = FALSE) {
  if ("--help" %in% c(...))
    return(guild("api runs --help"))




  # --json option always shows all runs
  x <- guild("api", "runs", ..., stdout = TRUE)
  x <- paste0(x, collapse = '')
  df <- parse_json(x, simplifyVector = TRUE)

  if(identical(df, list()))
    return()

  # TODO: guild should return something that's tz aware,
  #   This approach the best we can do is assume the dt string is
  #   in the user locale
  for(dt_name in c("started", "stopped"))
    df[[dt_name]] <- as.POSIXct(df[[dt_name]],
                                format = "%Y-%m-%d %H:%M:%OS")
  # df$time <- as.difftime(df$time, units = "secs")

  df$comments <- lapply(df$comments, as.character)

  # reorder columns for nicer printing.
  nms <- unique(c("shortId", "label", "flags", "scalars", names(df)))
  nms <- unique(c(nms, "env", "id"), fromLast = TRUE)
  df <- df[nms]

  df$scalars <- lapply(df$scalars, tibble::as_tibble)

  # these columna are dataframes
  for(nm in c("flags"))
    df[[nm]] <- tibble::as_tibble(df[[nm]])

  # these columns are lists of dataframes
  for(nm in c("scalars", "files"))
    df[[nm]] <- lapply(df[[nm]], tibble::as_tibble)


  # df <- rapply(list(df), tibble::as_tibble,
  #              classes = "data.frame", how = "replace")[[1L]]


  if (isFALSE(full)) {
    # delete not useful columns
    df$opRef <- NULL
    df$command <- NULL
    df$env <- NULL
    df$files <- NULL

    # pluck just the relevant info
    # df$sourcecode <- df$sourcecode$files
    df$sourcecode <- NULL
  }

  df <- tibble::as_tibble(df)

  # df$scalars[[1]]
  #   $prefix
  #     observed ".guild" for stdout, "logs/train" "logs/validate" for tfevents
  #   $tag
  #     observed "loss",  "epoch_accuracy", etc.

  df
}


#' Get Full set of runs scalars
#'
#' @param ... passed on go `guild()`
#'
#' @return a tibble with runs scalars
#' @export
runs_scalars <- function(...) {
  text <- guild("tensorboard --export-scalars -", ..., stdout = TRUE)
  readr::read_csv(I(text), col_types = "cccdd")
}

