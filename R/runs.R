

#' list guild runs
#'
#' @param ... additional arguments passed to `guild api runs`. Try
#'   `"--help"` to see options.
#'
#' @return a dataframe of runs
#' @export
#' @importFrom jsonlite parse_json
#' @importFrom rlang %|%
#' @examples
#' \dontrun{
#' withr::with_package("dplyr", {
#'
#' ## sort and filter using scalars
#' pluck_scalar <- function(scalar_summary_df, val, tag, prefix = ".guild") {
#'   if(!length(scalar_summary_df)) return(NA)
#'   out <- scalar_summary_df %>%
#'     filter(tag == {{ tag }}, prefix == {{ prefix }}) %>%
#'     pull({{ val }})
#'   if(length(out) < 1)
#'     out <- NA
#'   out
#' }
#'
#' runs <- ls_runs()
#' runs %>%
#'   rowwise() %>%
#'   mutate(
#'     min_val_loss =
#'       pluck_scalar(scalars, min_val, "epoch_loss", "logs/validation"),
#'     .after = label) %>%
#'   ungroup() %>%
#'   slice_min(min_val_loss, n = 5)
#'
#' })
#' }
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
  df[c("shortId", "time", "command", "files",
       "env", "sourcecode", "opRef")] <- NULL

  # additional coercion
  df$comments <- lapply(df$comments, as.character)
  df$tags     <- lapply(df$tags, as.character)


  ## `guild api runs` is missing some info, mainly a portable timestamp
  ## and marked status.
  # --json option always shows all runs, so don't pass `...`
  df2 <- guild("runs list --json", stdout = TRUE) |>
    paste0(collapse = "") |>
    parse_json(simplifyVector = TRUE)

  df2 <- df2[match(df$id, df2$id), ]

  stopifnot(identical(df$id, df2$id))

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
# ## TODO: think about how best to export `the resolve=TRUE` version of this.
# ## As a user-facing func it needs a better name/interface, probably should be a generic.
as_runs_selection <- function(x, resolve = FALSE) {
  # Any reason to explicitly resolve ids at this stage using
  # `guild("select", x, stdout = TRUE)` here?
  # `guild select` only returns 1 run at a time
  x <- if (is.data.frame(x)) (x[["id"]] %||% x[["run"]]) else x
  if(resolve)
    x <- guild("select --all", x, stdout = TRUE)
  x
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
#' @param location A directory where to place the runs, or find the runs.
#' @param ... passed on to guild
#' @param move bool, whether the runs should be moved or copied by the import or export operation.
#' @param copy_resources whether run resources should be also copied. If
#'   `FALSE`, (the default), run resources in the run directory will be
#'   symlinks to a guild managed storage location.
#'
#' @return NULL, invisibly
#' @export
runs_export <- function(runs = NULL, location, ...,
                        move = FALSE, copy_resources = FALSE) {
  guild("export --yes",
        if (move) "--move",
        if (copy_resources) "--copy-resources",
        ..., location, as_runs_selection(runs))
  invisible(runs)
}


#' @rdname runs_export
#' @export
runs_import <- function(runs = NULL, location, ...,
                        move = FALSE, copy_resources = FALSE) {
  guild("import --yes",
        if (move) "--move",
        if (copy_resources) "--copy-resources",
        ..., location, as_runs_selection(runs))
}

# TODO: runs_export() and runs_import() should return something useful
# for composition with %>%.





#' Delete runs
#'
#' @details
#' - `runs_delete()` moves runs into a guild managed "trash" directory.
#'
#' - `runs_restore()` moves runs back into the main guild managed "runs"
#' directory.
#'
#' - `runs_purge()` permanently delete runs from "trash" directory. Only
#' deleted runs can be purged.
#'
#' @param runs a runs selection
#' @param ... passed on to `guild()`
#'
#' @note To see deleted runs, do `guildai:::guild("runs list --deleted")`
#'   or `fs::dir_info(Sys.getenv(`
#'
#' @export
runs_delete <- function(runs = NULL, ...) {
  guild("runs delete --yes", ..., as_runs_selection(runs))
  invisible(runs)
}

#' @export
#' @rdname runs_delete
runs_purge <- function(runs = NULL, ...) {
  guild("runs purge --yes", ..., as_runs_selection(runs))
  invisible(runs)
}

#' @export
#' @rdname runs_delete
runs_restore <- function(runs = NULL, ...) {
  guild("runs restore", ..., as_runs_selection(runs))
  invisible(runs)
}



#' Annotate runs
#'
#' @param runs a runs selection
#' @param label,tag,comment string
#' @param ...  passed on to `guild()`
#' @param action what action to take respective to existing tags. "delete" is an alias
#'
#' @export
#' @examples
#' if(FALSE) {
#'
#' ls_runs(1) %>% runs_tag("foo")
#' ls_runs(1)$tags
#' ls_runs(1) %>% runs_tag("bar")
#' ls_runs(1)$tags
#' ls_runs(1) %>% runs_tag("foo", action = "remove")
#' ls_runs(1) %>% runs_tag(action = "clear")
#'
#' ## pass through options to `guild tag` cli subcommand
#' ls_runs(1) %>% runs_tag("--help")
#' ls_runs(1) %>% runs_tag("--add" = "foo", "--delete" = "bar")
#'
#' }
runs_label <- function(runs = NULL, label, ...) {
  if (is.null(label))
    guild("label --yes --clear", ..., as_runs_selection(runs))
  else
    guild("label --yes", "--set" = label, ...,
          as_runs_selection(runs))
  # TODO: update label if `runs` is a df
  invisible(runs)
}

#' @export
#' @rdname runs_label
runs_tag <- function(runs = NULL, tags, ..., action = c("add", "set", "remove", "clear", "delete")) {
  action <- match.arg(action)
  # "remove" is alias for "delete"; use terminology consistent with runs_label()
  if (action == "remove")
    action <- "delete"

  runs_selection <-  as_runs_selection(runs)

  if(action %in% c("clear", "set"))
    guild("tag --yes", ..., "--clear", runs_selection)

  if (action == "clear")
    return(invisible(runs_selection))

  if (missing(tags))
    tags <- NULL

  if(!is.null(tags)) {
    tags <- as.list(tags)
    names(tags) <- rep(paste0("--", action), length(tags))
  }
  guild("tag --yes", ..., tags, runs_selection)
  invisible(runs_selection)
}


#' @export
#' @rdname runs_label
runs_mark <- function(runs = NULL, ..., clear = FALSE) {
  guild("mark --yes", if (clear) "--clear", ..., as_runs_selection(runs))
  invisible(runs)
}

#' @export
#' @rdname runs_label
runs_comment   <- function(runs = NULL, comment = NULL, ..., clear = FALSE) {
  runs_selection <- as_runs_selection(runs)
  if(clear)
    guild("runs comment --clear", ..., runs_selection)

  if(length(comment))
    guild("runs comment", "--add" = paste0(comment, collapse = "\n"),
          ..., runs_selection)
  invisible(runs)
}



