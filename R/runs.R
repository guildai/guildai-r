

#' list guild runs
#'
#' @param ... additional arguments passed to `guild api runs`. Try
#'   `"--help"` to see options.
#'
#' @return a dataframe of runs
#' @export
#' @importFrom jsonlite parse_json
#' @importFrom rlang %|%
ls_runs <- function(...) {
  if ("--help" %in% c(...))
    return(guild("api runs --help"))

  df <- guild("api runs", ..., stdout = TRUE) |>
    paste0(collapse = "") |>
    parse_json(simplifyVector = TRUE)

  if(identical(df, list()))
    return()


  # drop some overly verbose info. All this
  # is easily accessible in run_dir/.guild/attrs users that need it.
  #
  df[c("shortId", "time", "command", "files",
       "env", "sourcecode", "opRef")] <- NULL

  # additional coersion
  df$comments <- lapply(df$comments, as.character)


  ## `guild api runs` is missing some info, mainly a portable timestamp
  ## and marked status.
  # --json option always shows all runs, so don't pass `...`
  df2 <- guild("runs list --json", stdout = TRUE) |>
    paste0(collapse = "") |>
    parse_json(simplifyVector = TRUE)

  df2 <- df2[match(df$id, df2$id), ]

  stopifnot(identical(df$id, df2$id))

  browser()

  # use epoch for timestamps
  df$started <- .POSIXct(df2$started/1000000)
  df$stopped <- .POSIXct(df2$stopped/1000000)
  df$marked <- df2$marked %|% FALSE


  # reorder columns for nicer printing.
  nms <- unique(c("label", "tags", "marked", "scalars", "flags",
                  names(df)))
  nms <- unique(c(nms, "dir", "id"), fromLast = TRUE)
  df <- df[nms]

  ## fix up scalars to make it convenient for other dplyr verbs


  df <- tibble::as_tibble(df)
  df[["flags"]] <- tibble::as_tibble(df[["flags"]])
  df[["scalars"]] <- lapply(df[["scalars"]], tibble::as_tibble)

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
#' ls_scalars(1) # scalars from most recent run
#' ls_scalars(1:2) # scalars form two most recent runs
#'
#' # pass in a dataframe of runs
#' ls_runs() %>%
#'   filter(flags$epochs > 5) %>%
#'   ls_scalars()
#' }
ls_scalars <- function(runs = NULL, ...) {

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



