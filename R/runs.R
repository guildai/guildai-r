

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
ls_runs <- function(..., full = FALSE) {
  if ("--help" %in% c(...))
    return(guild("api runs --help"))

  # --json option always shows all runs
  x <- guild("api runs", ..., stdout = TRUE)
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

# https://my.guild.ai/t/command-select/115
as_runs_selection <- function(x) {
  # Any reason to explicitly resolve ids at this stage using
  # `guild("select", x, stdout = TRUE)` here?
  if (is.data.frame(x)) x$id else x
}

#' Get full set of runs scalars
#' @param runs a runs selection
#' @param ... passed on go `guild()`
#'
#' @return a tibble with runs scalars
#' @export
#' @examples
#' \dontrun{
#' runs_scalars(1) # scalars from most recent run
#' runs_scalars(1:2) # scalars form two most recent runs
#'
#' # pass in a dataframe of runs
#' ls_runs() %>%
#'   filter(flags$epochs > 5) %>%
#'   runs_scalars()
#' }
runs_scalars <- function(runs = NULL, ...) {

  csv <- tempfile(fileext = ".csv")
  guild("tensorboard --export-scalars", csv,
        ..., as_runs_selection(runs))
  readr::read_csv(csv, col_types = "cccdd")
}



#' Move or copy runs
#'
#' @param runs A runs selection
#' @param location A directory where to place the runs
#' @param ... passed on to guild
#' @param copy_resources whether run resources should be also copied. If
#'   FALSE, (the default), run resources in the run directory will be
#'   symlinks to a guild managed storage location.
#'
#' @return NULL, invisibly
#' @export
runs_copy <- function(runs = NULL, location, ..., copy_resources = FALSE) {
  fs::dir_create(location)
  guild("export --yes", if (copy_resources) "--copy-resources",
        ..., location, as_runs_selection(runs))
}


#' @rdname runs_copy
#' @export
runs_move <- function(runs = NULL, location, ..., copy_resources = FALSE) {
  runs_copy(runs, location, "--move", ..., copy_resources = copy_resources)
}

# TODO: runs_copy() and runs_move() should return the created paths
# invisibly, or the resolved run id's invisibly.





#' Delete runs
#'
#' `runs_delete()` moves runs into a guild managed "trash" directory.
#' `runs_purge()` permanently delete runs from "trash" directory.
#'   Only deleted runs can be purged.
#' @param runs a runs selection
#' @param ... passed on to `guild()`
#'
#' @export
runs_delete <- function(runs = NULL, ...) {
  guild("delete --yes", ..., as_runs_selection(runs))
}

#' @export
#' @rdname runs_delete
runs_purge <- function(runs = NULL, ...) {
  guild("purge --yes", ..., as_runs_selection(runs))
}



#' Annotate runs
#'
#' @param runs a runs selection
#' @param label,tag string
#' @param ...  passed on to `guild()`
#' @param action what action to take w/ existing labels/tags
#'
#' @export
runs_label <- function(runs = NULL, label, ..., action = c("prepend", "append", "set", "remove", "clear")) {
  action <- match.arg(action)
  if (is.null(label) || action == "clear")
    guild("label --yes --clear", ..., as_runs_selection(runs))
  else
    guild("label --yes", paste0("--", action), label, ...,
          as_runs_selection(runs))
}

#' @export
#' @rdname runs_label
runs_tag <- function(runs = NULL, tag, ..., action = c("add", "remove", "clear"), sync_labels = FALSE) {
  action <- match.arg(action, choices = c("add", "remove", "delete", "clear"))
  # remove is alias for delete; use terminology consistent with runs_label()
  if(action == "remove")
    action <- "delete"
  runs <-  as_runs_selection(runs)

  if (!missing(tag)) {
    if (is.null(tag) || action == "clear")
      guild("tag --yes", ..., "--clear", runs)
    else
      guild("tag --yes", ..., paste0("--", action), label, runs)
  }
  if(sync_labels)
    guild("tag --yes", ..., "--sync-labels", runs)
}

#' @export
#' @rdname runs_label
runs_mark <- function(runs = NULL, ..., clear = FALSE) {
  guild("mark --yes", if(clear) "--clear", ..., as_runs_selection(runs))
}



