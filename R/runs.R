

#' list guild runs
#'
#' @param runs a runs specification.
#' @param ... additional arguments passed to `guild api runs`. Try
#'   `"--help"` to see options.
#'
#' @return a dataframe of runs
#' @export
#' @importFrom jsonlite parse_json
#' @importFrom rlang %|%
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr bind_rows bind_cols
#' @examples
#' \dontrun{
#' if(FALSE) {
#' withr::with_package("dplyr", {
#'
#' ls_runs() # get the full set of runs
#' ls_runs(1) # get the most recent run
#' ls_runs(1:3) # get the last 3 runs
#'
#' # some other examples for passing filter expressions
#' ls_runs(staged = TRUE) # list only staged runs
#' ls_runs(tag = c("convnet", "keras"), started = "last hour")
#' ls_runs(error = TRUE)
#'
#' #'
#' runs <- ls_runs()
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
#'   ls_scalars()
#'
#' # export the best run
#' runs %>%
#'   slice_max(scalars$test_accuracy) %>%
#'   runs_tag("best") %>%
#'   runs_export("./my-best-runs")
#'
#' }
#' })
#' }
ls_runs <- function(runs = NULL, ...) {
  df <- guild("api runs", ..., maybe_extract_run_ids(runs), stdout = TRUE) |>
    paste0(collapse = "") |>
    parse_json(simplifyVector = TRUE)

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
      # TODO?: include `step` as another scalar in this context,
      # maybe only if the user output it in stdout
      # if((step <- max(run_scalars$maxStep[run_scalars$prefix == ".guild"])) >= 0L) {
      #   out[["step"]] <- step
      #   append(path) <- ".guild"
      # }
      lapply(split.default(out, path), as_tibble)
    }))

  # unpack scalars parsed from stdout
  # leave scalars from run generated tfevent records packed
  from_stdout <- scalars[[".guild"]]
  scalars[[".guild"]] <- NULL
  ## TODO: check for name collisions between stdout keys and tfevent record paths,
  ## figure out something elegant. Right now ti
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


## Note on ls_runs()$scalars:
##
## We fixup `scalars` to make it more convenient to compose ls_runs() with dplyr::filter()
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
# In ls_runs() we simplify that list down to a single df with NA's for scalars in runs that don't exist,
# (like flags), so users can write: ls_runs() %>% filter(scalars$test_loss < .1)
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
#' @param ...  passed on to `guild`. Pass `"--help"` to see all options.
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
runs_comment <- function(runs = NULL, comment = NULL, ...,
                         delete = NULL, clear = FALSE) {
  if(is.null(comment) && is.null(delete) && isFALSE(clear) &&
     requireNamespace("rstudioapi", quietly = TRUE) &&
     rstudioapi::isAvailable()) {
    # Guild will attempt to launch EDITOR (e.g., vi), which will fail in the IDE
    # because the IDE doesn't pass through system() doesn't pass through
    # console commands to the tty.
    fi <- tempfile("guild-comment", fileext = ".txt")
    writeLines(c(strrep(" ", 75), character(23)), fi)
    mtime <- file.mtime(fi)
    utils::file.edit(fi, title = "Guild Comment")
    if(mtime != file.mtime(fi))
      comment <- trimws(paste0(trimws(readLines(fi)), collapse = "\n"))
    unlink(fi)
    if(!nzchar(comment))
      return(message("Aborting due to an empty comment."))
  }
  if(length(comment) > 1)
    comment <- paste0(comment, collapse = "\n")
  guild("runs comment --yes",
        list(clear = clear, delete = delete, add = comment, ...),
        maybe_extract_run_ids(runs))
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
#' @param all Return all matching runs, not just the latest.
#'
#' @return A character vector of run ids.
#' @export
#'
#' @examples
#' if(FALSE) {
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
#' # resolve_run_ids() uses the same selection rules and syntax as ls_runs()
#' stopifnot(identical(
#'   resolve_run_ids(1),
#'   ls_runs(1)$id
#' ))
#' }
resolve_run_ids <- function(runs = NULL, ..., all = TRUE) {
  selection <- maybe_extract_run_ids(runs)
  if(...length() || identical(selection, runs))
    selection <- guild("select", list(all = all, ..., selection),
                       stdout = TRUE)
  selection
}



maybe_extract_run_ids <- function(x) {
  if (is.list(x))
    if(!is.null(id <- x[["id"]] %||% x[["run"]]))
      return(id)
  x
}

# TODO: support for multiple `flags-dest` file paths, as a yaml list

# TODO: `guild api runs --operation 'train.R'` doesn't work, I think because
# guild doesn't know about this operation in this context because it's not
# listed in the guildfile. Not sure what the best way to resolve this is, or
# even if we should...


# TODO: tidyr::unpack() loses data if names collide.
# df <- ls_runs()
# df$flags$test_accuracy <- 1
# ls_runs() %>%  select(id, flags, scalars) %>% tidyr::unpack(c(flags, scalars))
