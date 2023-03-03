

#' Get runs information
#'
#' Returns a dataframe with information about the guild runs stored in guild
#' home. Guild home is determined either by consulting the env var
#' `Sys.getenv("GUILD_HOME")`, or if unset, by looking for a `.guild`
#' directory, starting from the current working directory and walking up
#' parent directories up to `~` or `/`.
#'
#' @details
#'
#' Guild has support for a custom filter expression syntax. This syntax is
#' primarily useful in the terminal, and R users will generally prefer to
#' filter the returned dataframe directly using `dplyr::filter()` or `[`.
#' Nevertheless, R users can supply guild filter expressions here as well.
#'
#'
#' ### Filter by Expression
#'
#' Use `filter` to limit runs that match a filter
#' expressions. Filter expressions compare run attributes, flag
#' values, or scalars to target values. They may include multiple
#' expressions with logical operators.
#'
#' For example, to match runs with flag `batch-size` equal to 100
#' that have `loss` less than 0.8, use:
#'
#'     runs_info(filter = "batch-size = 10 and loss < 0.8")
#'
#' Target values may be numbers, strings or lists containing numbers
#' and strings. Lists are defined
#' using square braces where each item is separated by a comma.
#'
#' Comparisons may use the following operators:
#' '=', '!=', '<', '<=', '>', '>='.
#'
#' Text comparisons may use 'contains' to test
#' for case-insensitive string membership. A value may be tested for
#' membership or not in a list using 'in' or 'not in'
#' respectively. An value may be tested for undefined using 'is
#' undefined' or defined using 'is not undefined'.
#'
#' Logical operators include 'or' and 'and'. An expression may be
#' negated by preceding it with 'not'. Parentheses may be used to
#' control the order of precedence when expressions are evaluated.
#'
#' If a value reference matches more than one type of run information
#' (e.g. a flag is named 'label', which is also a run attribute), the
#' value is read in order of run attribute, then flag value, then
#' scalar. To disambiguate the reference, use a prefix `attr:`,
#' `flag:`, or `scalar:` as needed. For example, to filter using a
#' flag value named 'label', use 'flag:label'.
#'
#' Other examples:
#'
#'     "operation = train and acc > 0.9"
#'     "operation = train and (acc > 0.9 or loss < 0.3)"
#'     "batch-size = 100 or batch-size = 200"
#'     "batch-size in [100,200]"
#'     "batch-size not in [400,800]"
#'     "batch-size is undefined"
#'     "batch-size is not undefined"
#'     "label contains best and operation not in [test,deploy]"
#'     "status in [error,terminated]"
#'
#' **NOTE:** Comments and tags are not supported in filter
#' expressions at this time. Use `comment` and `tag` options
#' along with filter expressions to further refine a selection.
#'
#' ### Filter by Run Start Time
#'
#' Use `started` to limit runs to those that have started within a
#' specified time range.
#'
#'     runs_info(started = 'last hour')
#'
#' You can specify a time range using several different forms:
#'
#'
#'     "after DATETIME"
#'     "before DATETIME"
#'     "between DATETIME and DATETIME"
#'     "last N minutes|hours|days"
#'     "today|yesterday"
#'     "this week|month|year"
#'     "last week|month|year"
#'     "N days|weeks|months|years ago"
#'
#' `DATETIME` may be specified as a date in the format ``YY-MM-DD``
#' (the leading ``YY-`` may be omitted) or as a time in the format
#' ``HH:MM`` (24 hour clock). A date and time may be specified
#' together as `DATE TIME`.
#'
#' When using ``between DATETIME and DATETIME``, values for
#' `DATETIME` may be specified in either order.
#'
#' When specifying values like ``minutes`` and ``hours`` the trailing
#' ``s`` may be omitted to improve readability. You may also use
#' ``min`` instead of ``minutes`` and ``hr`` instead of ``hours``.
#'
#' Examples:
#'
#'
#'     "after 7-1"
#'     "after 9:00"
#'     "between 1-1 and 4-30"
#'     "between 10:00 and 15:00"
#'     "last 30 min"
#'     "last 6 hours"
#'     "today"
#'     "this week"
#'     "last month"
#'     "3 weeks ago"
#'
#'
#' ### Filter by Run Status
#'
#' Runs may also be filtered by specifying one or more status
#' filters: `running`, `completed`, `error`, and
#' `terminated`. These may be used together to include runs that
#' match any of the filters. For example to only include runs that
#' were either terminated or exited with an error, use
#'
#'     runs_info(terminated = TRUE, error = TRUE)
#'
#' Status filters are applied before `RUN` indexes are resolved. For
#' example, a run index of ``1``
#' (as in, `runs_info(1, terminated = TRUE, error = TRUE)` is the latest run
#' that matches the status filters.
#'
#'
#' @param runs a runs specification.
#' @param ... passed on to `guild`.
#' @param filter (character vector) Filter runs using a guild filter expression. See details section.
#' @param operation (character vector) Filter runs with matching `operation`s. A run is
#' only included if any part of its full operation name matches the value.
#' @param label (character vector) Filter runs with matching labels.
#' @param unlabeled (bool) Filter only runs without labels.
#' @param tag (character vector) Filter runs with `tag`.
#' @param comment (character vector) Filter runs with comments matching.
#' @param marked (bool) Filter only marked runs.
#' @param unmarked (bool) Filter only unmarked runs.
#' @param started (string) Filter only runs started within RANGE. See details for valid time ranges.
#' @param digest (string) Filter only runs with a matching source code digest.
#' @param running (bool) Filter only runs that are still running.
#' @param completed (bool) Filter only completed runs.
#' @param error (bool) Filter only runs that exited with an error.
#' @param terminated (bool) Filter only runs terminated by the user.
#' @param pending (bool) Filter only pending runs.
#' @param staged (bool) Filter only staged runs.
#' @param deleted (bool) Show deleted runs.
#' @param include_batch (bool) Include batch runs.
#'
#' @return A dataframe (tibble) of runs
#' @export
#' @importFrom jsonlite parse_json
#' @importFrom rlang %|%
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr bind_rows bind_cols
#' @examples
#' \dontrun{
#' withr::with_package("dplyr", {
#'
#' runs_info() # get the full set of runs
#' runs_info(1) # get the most recent run
#' runs_info(1:3) # get the last 3 runs
#'
#' # some other examples for passing filter expressions
#' runs_info(staged = TRUE) # list only staged runs
#' runs_info(tag = c("convnet", "keras"), started = "last hour")
#' runs_info(error = TRUE)
#'
#' runs <- runs_info()
#'
#' # filter down the runs list to ones of interest
#' runs <- runs %>%
#'   filter(exit_status == 0) %>% # run ended without an error code
#'   filter(scalars$test_accuracy > .8) %>%
#'   filter(flags$epochs > 10) %>%
#'   arrange(scalars$test_loss) %>%
#'   select(id, flags, scalars)
#'
#' # retrieve full scalars history from the runs of interest
#' runs$id %>%
#'   runs_scalars()
#'
#' # export the best run
#' best_runs_dir <- tempfile()
#' dir.create(best_runs_dir)
#' runs %>%
#'   slice_max(scalars$test_accuracy) %>%
#'   runs_tag("best") %>%
#'   runs_export(best_runs_dir)
#'
#' })
#' }
runs_info <-
function(runs = NULL, ...,
         filter = NULL,
         operation = NULL,
         label = NULL,
         unlabeled = NA,
         tag = NULL,
         comment = NULL,
         marked = NA,
         unmarked = NA,
         started = NULL,
         digest = NULL,
         running = NA,
         completed = NA,
         error = NA,
         terminated = NA,
         pending = NA,
         staged = NA,
         deleted = NA,
         include_batch = NA) {

  df <- guild("api runs", list(...),
              mget(setdiff(ls(), "runs")),
              maybe_extract_run_ids(runs),
              stdout = TRUE)

  if(!is.null(attr(df, "status", TRUE)))
    stop("guild error (status code ", attr(df, "status", TRUE), ")")

  df <- parse_json(paste0(df, collapse = ""),
                   simplifyVector = TRUE)
  if(identical(df, list()))
    return()

  # drop some overly verbose info. All this
  # is easily accessible in run_dir/.guild/attrs
  # for users that need it.
  df[c("shortId", "time", "command", "files",
       "env", "sourcecode", "opRef", "otherAttrs")] <- NULL

  # additional coercion
  df$tags     <- lapply(df$tags, as.character)
  df$comments <- lapply(df$comments, function(x) {
    if(!length(x))
      x <- tibble(body = character(), host = character(),
                  time = double(), user = character())
    x$time <- .POSIXct(x$time / 1000000)
    as_tibble(x)
  })
  for(tm in c("started", "stopped"))
    df[[tm]] <- .POSIXct(df[[tm]] / 1000000)

  # See "Note" below
  scalars <-
    bind_rows(lapply(df$scalars, function(run_scalars) {
      if (!length(run_scalars))
        return(tibble(.rows = 1L))
      out <- as.list(run_scalars$lastVal)
      names(out) <- run_scalars$tag
      path <- str_drop_prefix(run_scalars$prefix, "logs/")
      lapply(split.default(out, path), as_tibble)
    }))

  # unpack scalars parsed from stdout
  # leave scalars from run generated tfevent records packed
  from_stdout <- scalars[[".guild"]]
  scalars[[".guild"]] <- NULL
  # TODO: better solution for name collisions in scalars between stdout
  # keys and tfevent record paths. Current behavior is ugly, but not
  # catastrophic. dplyr::bind_cols() automatically renames vars as
  # needed to make everything unique.
  #
  # if(length(colliding_names <-
  #      intersect(names(from_stdout), names(scalars))) {
  #   ???
  # }
  df$scalars <- dplyr::bind_cols(from_stdout, scalars)

  # reorder columns for nicer printing.
  nms <- unique(c("label", "tags", "marked", "flags", "scalars",
                  names(df)))
  nms <- unique(c(nms, "dir", "id"), fromLast = TRUE)
  df <- df[nms]

  # rename some fields for consistent camel_case.
  nms[nms == "exitStatus"] <- "exit_status"
  nms[nms == "projectDir"] <- "project_dir"
  # nms[nms == "dir"] <- "run_dir"
  names(df) <- nms

  df[["flags"]] <- as_tibble(df[["flags"]])
  df[["scalars"]] <- as_tibble(df[["scalars"]])
  df <- as_tibble(df)

  df
}


# TODO(maybe): make scalars a flat namespace with "." separating dir /
# e.g., something like runs_info()$scalars$"train/accuracy"
# runs_info()$scalars$train.accuracy

## Note on runs_info()$scalars:
##
## We fixup `scalars` to make it more convenient to compose runs_info() with dplyr::filter()
# By default, system("guild api runs") returns something that simplifies
# to a list of dataframes that look like this:
## # A tibble: 4 × 14
##   run                              prefix tag           firstVal firstStep lastVal lastStep minVal minStep maxVal maxStep avgVal total count
##   <chr>                            <chr>  <chr>            <dbl>     <int>   <dbl>    <int>  <dbl>   <int>  <dbl>   <int>  <dbl> <dbl> <int>
## 1 a1adb737137240bb92adf6e8bbd88ff6 .guild test_accuracy    0.887         0   0.887        0  0.887       0  0.887       0  0.887 0.887     1
## 2 a1adb737137240bb92adf6e8bbd88ff6 .guild test_loss        0.339         0   0.339        0  0.339       0  0.339       0  0.339 0.339     1
## 3 a1adb737137240bb92adf6e8bbd88ff6 train  test_accuracy    0.887         0   0.887        0  0.887       0  0.887       0  0.887 0.887     1
## 4 a1adb737137240bb92adf6e8bbd88ff6 train  test_loss        0.339         0   0.339        0  0.339       0  0.339       0  0.339 0.339     1
#
# In runs_info() we simplify that list down to a single df with NA's for scalars in runs that don't exist,
# (like flags), so users can write: runs_info() %>% filter(scalars$test_loss < .1)
#
# An alternative explored was to make the scalar summaries easily "hoist-able", like this:
# df %>%
#   tidyr::hoist(scalars, final_test_loss = c("test_loss", "lastVal")) %>%
#   filter(final_test_loss > .3)
#
# Here is a snippet that makes it hoistable, in case it's needed later:
# df$scalars %<>%
#   lapply(function(scalars_df) {
#     prefix <- unique(scalars_df$prefix)
#     names(prefix) <- prefix
#     out <- lapply(prefix, function(p) {
#       sc <- scalars_df %>% filter(prefix == p)
#       sc[c("run", "prefix", "tag")] <- NULL
#       sc <- lapply(seq_len(nrow(sc)), function(r) sc[r,])
#       names(sc) <- sc_og$tag
#       sc
#     })
#     no_prefix <- out$.guild
#     out$.guild <- NULL
#     out <- c(no_prefix, out)
#     out
#   })


#' Get full set of runs scalars
#' @param runs a runs selection
#' @param ... passed on go `guild`
#'
#' @return A dataframe (tibble) of runs
#' @export
#' @examples
#' \dontrun{
#' runs_scalars(1) # scalars from most recent run
#' runs_scalars(1:2) # scalars form two most recent runs
#'
#' # pass in a dataframe of runs
#' runs_info() %>%
#'   filter(flags$epochs > 5) %>%
#'   runs_scalars()
#' }
runs_scalars <- function(runs = NULL, ...) {
  csv <- tempfile(fileext = ".csv")
  guild("tensorboard --export-scalars", csv,
        list(...), maybe_extract_run_ids(runs))
  out <- readr::read_csv(csv, col_types = "cccdd", na = character())
  path <- out$path
  path[path == ".guild"] <- NA_character_
  path <- str_drop_prefix(path, "logs/")
  out$path <- path
  out
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
#' @return The value supplied to the `runs` argument, invisibly.
#' @export
runs_export <- function(runs = NULL, location, ...,
                        move = FALSE, copy_resources = FALSE) {
  guild("export --yes",
        if (isTRUE(move)) "--move",
        if (isTRUE(copy_resources)) "--copy_resources",
        list(...), location, maybe_extract_run_ids(runs))
  invisible(runs)
}


#' @rdname runs_export
#' @export
runs_import <- function(runs = NULL, location, ...,
                        move = FALSE, copy_resources = FALSE) {
  guild("import --yes",
        if (isTRUE(move)) "--move",
        if (isTRUE(copy_resources)) "--copy_resources",
        list(...), location, maybe_extract_run_ids(runs))
  invisible(runs)
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
#' @param ... passed on to `guild`
#'
#' @note To see deleted runs, do `guildai:::guild("runs list --deleted")`
#'   (`runs_info("--deleted")` supported soon)
#'
#' @return The value supplied to the `runs` argument, invisibly.
#'
#' @export
runs_delete <- function(runs = NULL, ...) {
  guild("runs delete --yes", list(...), maybe_extract_run_ids(runs))
  invisible(runs)
}

#' @export
#' @rdname runs_delete
runs_purge <- function(runs = NULL, ...) {
  guild("runs purge --yes", list(...), maybe_extract_run_ids(runs))
  invisible(runs)
}

#' @export
#' @rdname runs_delete
runs_restore <- function(runs = NULL, ...) {
  guild("runs restore", list(...), maybe_extract_run_ids(runs))
  invisible(runs)
}



#' Annotate runs
#'
#' @details Annotation types and their recommended uses:
#'
#' * __labels__: short, single line descriptions tailored for readability,
#'   not programmatic consumption. Labels are presented prominently in
#'   `guild_view()` and other run views.
#'
#' * __tags__: short single-token strings. Tags can be used for organizing, grouping,
#'   and filtering runs.
#'
#' * __comments__: longer (potentially multi-paragraph) descriptions of the
#'   run. Guild stores and presents run comments as log entries,
#'   complete with timestamps and author info.
#'
#' * __marks__: A boolean attribute of a run (a run can be marked or unmarked).
#'   Marked runs are primarily used to declare a run as the preferred
#'   source for resolving an operation dependency. If a operation
#'   declares a dependency on another operation, and one of the
#'   dependent operation runs is marked, the marked run is used rather
#'   than the latest run for resolving the dependency. Marks can also be
#'   a convenient mechanism for ad-hoc filtering operations, but in
#'   general, tags are preferred over marks for this.
#'
#' @param runs a runs selection
#' @param label,comment a string
#' @param add,remove a character vector of tags to add or remove
#' @param delete integer vector, which comment(s) to delete,
#'   corresponding to the row number(s) in the dataframe found at
#'   `runs_info()$comments`.
#' @param clear bool, whether to clear the existing tags/comments/label.
#' @param ...  passed on to `guild`. Pass `"--help"` to see all options.
#'
#' @note `runs_comment()` will open up an editor if `comment` is not
#'   supplied.
#'
#' @return The value supplied to the `runs` argument, invisibly.
#' @export
#' @examples
#' \dontrun{
#' runs_info(1) %>% runs_tag(clear = TRUE)
#' runs_info(1) %>% runs_tag("foo")
#' runs_info(1)$tags
#' runs_info(1) %>% runs_tag("bar")
#' runs_info(1)$tags
#' runs_info(1) %>% runs_tag(remove = "foo")
#' runs_info(1)$tags
#' runs_info(1) %>% runs_tag("baz", clear = TRUE)
#' runs_info(1)$tags
#'
#' ## pass through options to `guild tag` cli subcommand
#' runs_tag("--help")
#' }
runs_label <- function(runs = NULL, label = NULL, ..., clear = FALSE) {
  guild("label --yes",
        if (clear) "--clear",
        "--set" = label,
        list(...), maybe_extract_run_ids(runs))
  invisible(runs)
  # TODO: update label if `runs` is a df
}


#' @export
#' @rdname runs_label
runs_tag <- function(runs = NULL, add = NULL, ..., remove = NULL, clear = FALSE) {
  guild("tag --yes",
        if (clear) "--clear",
        "--delete" = remove,
        "--add" = add,
        list(...), maybe_extract_run_ids(runs))
  invisible(runs)
}


#' @export
#' @rdname runs_label
runs_mark <- function(runs = NULL, ..., clear = FALSE) {
  guild("mark --yes", if (clear) "--clear", list(...),
        maybe_extract_run_ids(runs))
  invisible(runs)
}


#' @export
#' @rdname runs_label
runs_comment <- function(runs = NULL, comment = NULL, ...,
                         delete = NULL, clear = FALSE) {
  if(is.null(comment) && is.null(delete) && isFALSE(clear) &&
     is_rstudio()) {
    comment <- edit_comment()
    if(is.null(comment) || !nzchar(comment))
      return(message("Aborting due to an empty comment."))
  }
  if(length(comment) > 1)
    comment <- paste0(comment, collapse = "\n")
  guild("runs comment --yes",
        if (clear) "--clear",
        list(delete = delete, add = comment, ...),
        maybe_extract_run_ids(runs))
  invisible(runs)
}

is_rstudio <- function() {
  identical(.Platform$GUI, "RStudio")
}


edit_comment <- function() {
  # Guild will attempt to launch EDITOR (e.g., vi), which will fail in the IDE
  # because the IDE doesn't pass user console input to a foreground
  # system() process (nor even run the process in a tty).

  fi <- tempfile("guild-comment", fileext = ".txt")
  on.exit(unlink(fi))
  # write some whitespace to trick the IDE into drawing a bigger edit dialog.
  # otherwise it's too small by default and not resizable
  writeLines(c(strrep(" ", 75), character(23)), fi)
  mtime <- file.mtime(fi)
  utils::file.edit(fi, title = "Guild Comment")
  if(mtime != file.mtime(fi))
    trimws(paste0(trimws(readLines(fi)), collapse = "\n"))
  else
    NULL
}

# TODO: use edit_comment() in guild_run()


#' Resolve run ids
#'
#' This is a equivalent to `runs_info(...)$id`, implemented more
#' efficiently.
#'
#' guild supports a rich syntax for runs selection throughout the api.
#' The same selection syntax is shared by the `runs_*` family of
#' functions: `runs_info()`, `runs_scalars()`, `runs_comment()`,
#' `runs_label()`, `runs_mark()`, `runs_tag()`
#' `runs_delete()`,`runs_purge()`, `runs_restore()`, `runs_export()`,
#' `runs_import()`.
#'
#' @param runs a runs selection. If a data.frame, the columns `id` or
#'   `run` are used as the run id. Otherwise, the arguments are
#'   transformed into a character vector of cli arguments, and passed on
#'   to `guild` as a runs filter selection. Wrap the string in `I()` to
#'   avoid quoting the argument for the shell.
#' @param ... Other arguments passed on to `guild`
#' @param all Return all matching runs. If `FALSE`, it returns the
#'   singly most recent run matching the selection criteria.
#'
#' @note You can call `Sys.setenv(GUILD_DEBUG_R = 1)` to see what system
#'   calls to the `guild` executable are made. This is useful when
#'   looking to understand how R arguments are transformed into a cli
#'   system call.
#'
#' @return A character vector of run ids.
#' @export
#'
#' @examples
#' \dontrun{
#' resolve_run_ids() # returns all run ids.
#' resolve_run_ids(1) # last run
#' resolve_run_ids(1:2) # last 2 runs
#' resolve_run_ids(1:2, operation = "train.py")
#'
#' # three ways of getting ids for the currently staged or running runs
#' resolve_run_ids(staged = TRUE, running = TRUE)
#' resolve_run_ids("--staged", "--running")
#' resolve_run_ids(c("--staged", "--running"))
#' resolve_run_ids(I("--staged --running"))
#'
#' # resolve_run_ids() uses the same selection rules and syntax as runs_info()
#' stopifnot(identical(
#'   resolve_run_ids(),
#'   runs_info()$id
#' ))
#' }
resolve_run_ids <- function(runs = NULL, ..., all = TRUE) {
  selection <- maybe_extract_run_ids(runs)
  if(...length() || identical(selection, runs))
    selection <- guild("select", if(isTRUE(all)) "--all",
                       list(..., selection),
                       stdout = TRUE)
  selection
}



maybe_extract_run_ids <- function(x) {
  if (is.list(x))
    if(!is.null(id <- x[["id"]] %||% x[["run"]]))
      return(unique(id))
  x
}


# TODO: tidyr::unpack() loses data if names collide.
# df <- runs_info()
# df$flags$test_accuracy <- 1
# runs_info() %>% select(id, flags, scalars) %>% tidyr::unpack(c(flags, scalars))
