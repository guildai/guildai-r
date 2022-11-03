

#' list guild runs
#'
#' @param ... additional arguments passed to `guild api runs`. Try
#'   `"--help"` to see options.
#'
#' @return a dataframe of runs
#' @export
#' @importFrom jsonlite parse_json
#' @importFrom rlang %|%
#' @importFrom tibble tibble as_tibble
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
  df$tags     <- lapply(df$tags, as.character)
  df$comments <- lapply(df$comments, function(x) {
    if(!length(x))
      x <- tibble(body = character(), host = character(),
                  time = double(), user = character())
    x$time <- .POSIXct(x$time/1000000)
    as_tibble(x)
  })


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



#' Get full set of runs scalars
#' @param runs a runs selection
#' @param ... passed on go `guild`
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
        ..., maybe_extract_run_ids(runs))
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
        ..., location, maybe_extract_run_ids(runs))
  invisible(runs)
}


#' @rdname runs_export
#' @export
runs_import <- function(runs = NULL, location, ...,
                        move = FALSE, copy_resources = FALSE) {
  guild("import --yes",
        if (move) "--move",
        if (copy_resources) "--copy-resources",
        ..., location, maybe_extract_run_ids(runs))
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
#'   (`ls_runs("--deleted")` supported soon)
#'
#' @export
runs_delete <- function(runs = NULL, ...) {
  guild("runs delete --yes", ..., maybe_extract_run_ids(runs))
  invisible(runs)
}

#' @export
#' @rdname runs_delete
runs_purge <- function(runs = NULL, ...) {
  guild("runs purge --yes", ..., maybe_extract_run_ids(runs))
  invisible(runs)
}

#' @export
#' @rdname runs_delete
runs_restore <- function(runs = NULL, ...) {
  guild("runs restore", ..., maybe_extract_run_ids(runs))
  invisible(runs)
}



#' Annotate runs
#'
#' @param runs a runs selection
#' @param label,comment a string
#' @param add,remove a character vector of tags to add or remove
#' @param delete integer vector, which comment(s) to delete, corresponding to
#'   the row number(s) in the dataframe found at `ls_runs()$comments`.
#' @param clear bool, whether to clear the existing tags/comments/label.
#' @param ...  passed on to `guild`
#'
#' @note `runs_comment()` will open up an editor if `comment` is not supplied.
#'
#' @export
#' @examples
#' if(FALSE) {
#'
#' ls_runs(1) %>% runs_tag(clear = TRUE)
#' ls_runs(1) %>% runs_tag("foo")
#' ls_runs(1)$tags
#' ls_runs(1) %>% runs_tag("bar")
#' ls_runs(1)$tags
#' ls_runs(1) %>% runs_tag(remove = "foo")
#' ls_runs(1)$tags
#' ls_runs(1) %>% runs_tag("baz", clear = TRUE)
#' ls_runs(1)$tags
#'
#' ## pass through options to `guild tag` cli subcommand
#' runs_tag(NULL, NULL, "--help")
#' ls_runs(1) %>% runs_tag("--add" = c("foo", "bar"))
#' ls_runs(1) %>% runs_tag("--add" = "baz", "--delete" = "bar")
#' ls_runs(1)$tags
#'
#' }
runs_label <- function(runs = NULL, label = NULL, ..., clear = FALSE) {
  guild("label --yes",
        if (clear) "--clear",
        "--set" = label,
        ..., maybe_extract_run_ids(runs))
  invisible(runs)
  # TODO: update label if `runs` is a df
}


#' @export
#' @rdname runs_label
runs_tag <- function(runs = NULL, add = NULL, ..., remove = NULL, clear = FALSE) {
  guild("tag --yes",
        if(clear) "--clear",
        "--delete" = remove,
        "--add" = add,
        ..., maybe_extract_run_ids(runs))
  invisible(runs)
}


#' @export
#' @rdname runs_label
runs_mark <- function(runs = NULL, ..., clear = FALSE) {
  guild("mark --yes", if (clear) "--clear", ..., maybe_extract_run_ids(runs))
  invisible(runs)
}


#' @export
#' @rdname runs_label
runs_comment <- function(runs = NULL, comment = NULL, ..., delete = NULL, clear = FALSE) {
  guild("runs comment --yes",
        if (clear) "--clear",
        "--delete" = delete,
        "--add" = comment,
        ..., maybe_extract_run_ids(runs))
  invisible(runs)
}



#' Resolve run ids
#'
#' `guild` supports a rich syntax for runs selection throughout the api. The
#' same selection syntax is shared by the `ls_*` and `runs_*` families of
#' functions: `ls_runs()`, `ls_scalars()`, `runs_comment()`,
#' `runs_label()`, `runs_mark()`, `runs_tag()`
#' `runs_delete()`,`runs_purge()`, `runs_restore()`, `runs_export()`,
#' `runs_import()`.
#'
#' @param runs a runs selection. If a data.frame, the columns `id` or `run`
#'   are used as the run id. Otherwise, the argument is coerced to character
#'   vector, and passed on to `guild` as a runs filter selection. Wrap the
#'   string in `I()` to avoid quoting the argument for the shell.
#' @param ... Other arguments passed on to `guild`
#'
#' @return A character vector of run ids.
#' @export
#'
#' @examples
#' if(FALSE) {
#' resolve_run_ids() # returns all run ids.
#' resolve_run_ids(1) # last run
#' resolve_run_ids(1:2) # last 2 runs
#' resolve_run_ids(1:2, "--operation" = "train.py")
#'
#' # three ways of getting ids for the currently staged or running runs
#' resolve_run_ids("--staged", "--running")
#' resolve_run_ids(c("--staged", "--running"))
#' resolve_run_ids(I("--staged --running"))
#'
#' # resolve_run_ids() uses the same selection rules and syntax as ls_runs()
#' stopifnot(identical(
#'   resolve_run_ids(1),
#'   ls_runs(1)$id
#' ))
#' }
resolve_run_ids <- function(runs = NULL, ...) {
  selection <- maybe_extract_run_ids(runs)
  if(...length() || identical(selection, runs))
    selection <- guild("select --all", ..., selection, stdout = TRUE)
  selection
}



maybe_extract_run_ids <- function(x) {
  if (is.list(x))
    if(!is.null(id <- x[["id"]] %||% x[["run"]]))
      return(id)
  x
}
