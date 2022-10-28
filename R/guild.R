
#' @importFrom here here
#' @importFrom rlang is_string
guild <- function(...,
                  stdout = "", stderr = "",
                  home = Sys.getenv("GUILD_HOME", here(".guild")),
                  wait = TRUE) {

  args <- list(...)
  if(is_string(args[[1L]]))
    class(args[[1L]]) <- "AsIs" # I(args[[1L]])
  args <- .process_args(args)

  if(!is.null(home))
    args <- c("-H", shQuote(home), args)
  if(Sys.getenv("DEBUG") == "1")
    args <- c("-D", "5678", args)
  system2t(find_guild(), args,
           stdout = stdout, stderr = stderr,
           wait = wait)
}

.process_args <- function(x, name = "") {
  if(is.list(x)) # recurse
    return(unlist(.mapply(
      .process_args,
      list(x, names(x) %||% ""),
      NULL
    ), use.names = FALSE))

  # cast, to char, but preserve names
  storage.mode(x) <- "character"
  if (!inherits(x, "AsIs")) {
    needs_quoting <- !grepl("^[[:alnum:]_-]+$", x)
    x[needs_quoting] <- shQuote(x[needs_quoting])
  }

  if(is.null(names(x)) && identical(name, ""))
    return(x)

  # if args were supplied like "--path" = path:
  #   need to convert to c("--path", path)
  # if args were supplied like "--add" = c(tag1, tag2)
  #   recycle name, return c("--add", tag1, "--add", tag2)
  nms <- names(x) %||% rep(name, length(x))
  x <- as.list(x)
  for (i in seq_along(nms))
    if (isTRUE(nzchar(nm <- nms[[i]]) && startsWith(nm, "-")))
      x[i] <- list(c(nm, x[[i]]))

  unlist(x, use.names = FALSE)
}




# TODO: support for globals injection of `!expr foo` flags

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
#' @param label,tags optional strings used to label or tag experiments.
#'
#' @param ... passed through to [base::system2()]. Unnamed arguments are
#'   passed through to the guild executable. Arguments are automatically
#'   quoted with `shQuote()`, unless they are protected with `I()`. Additionally,
#'   named arguments to `system2()` can be supplied.
#' @inheritDotParams base::system2
#'
#' @return the return value from `system2()`, invisibly. This function is
#'   primarily called for its side effect.
#' @export
guild_run <- function(opspec = "train.R",
                      flags = NULL, ...,
                      wait = TRUE,
                      label = NULL,
                      tags = NULL,
                      echo = wait) {

  if (is.data.frame(flags)) {
    fi <- tempfile("guild-batch-flags-", fileext = ".yml")
    on.exit(unlink(fi))
    print.yaml(flags, fi, column.major = FALSE)
    flags <- paste0("@", fi)
  }

  if (!is.null(names(flags))) {
    # A scalar string for flags is passed through
    # otherwise, do some prep to build the command line arg
    flags <- lapply(flags, function(f) {
      x <- vapply(f,
                  function(fv) if(is.character(fv)) fv else encode_yaml(fv),
                  "", USE.NAMES = FALSE)
      if(length(x) > 1)
        x <- sprintf("[%s]", paste0(x, collapse = ","))
      x
    })
    flags <- sprintf("%s=%s", names(flags), unname(flags))
  }

  args <- c("--yes")
  if(is.character(label))
    append(args) <- c("--label", label)
  if(is.character(tags))
    append(args) <- as.vector(rbind("--tag", tags))

  cl <- call("guild", "run", args,
             opspec, quote(...), flags, wait = wait)
  if (!echo)
    cl$stdout <- cl$stderr <- FALSE
  if(Sys.getenv("DEBUGR") == "1")
    message("R> ", deparse1(cl))
  invisible(eval(cl))
}


#' Launch Guild Viewer
#'
#' @param runs an optional runs selection.
#' @param ... passed on to the `guild` executable. Pass `--help` to see options.
#' @param wait whether to block the R console while the application is active.
#'
#' @export
#' @examples
#' if(FALSE) {
#'   guild_view()
#'
#'   # see all supported options
#'   guild_view("--help")
#'
#'   # three valid ways of supplying args to the guild executable
#'   guild_view("--port" = "5678")
#'   guild_view("--port", "5678")
#'   guild_view(c("--port", "5678"))
#' }
guild_view <- function(runs = NULL, ..., wait = FALSE) {
  # TODO: use processx here?
  guild("view", ..., maybe_extract_run_ids(runs), wait = wait)
}



#' Copy run files into the current project working directory
#'
#' @param run a run selection
#' @param ... passed on to `guild`
#'
#' @export
#'
#' @examples
#' if(FALSE) {
#' guild_merge("--help")
#' ls_scalars() %>%
#'   dplyr::slice_max("epoch_acc") %>%
#'   guild_merge(I("--yes --replace"))
#' }
guild_merge <- function(run = NULL, ...) {
  guild("merge", ..., maybe_extract_run_ids(run))
}

# dummy place holder, because R CMD check otherwise complains:
#   Namespace in Imports field not imported from: ‘here’
#     All declared Imports should be used.
if(FALSE) {
  here::here()
}


# TODO: other subcommands to wrap + export:
# next:
#   download, publish,
# later:
#   remotes, remote, pull, push, runs pull, runs push,
#   remote start, remote stop, sys shutdown-timer, sys s3-sync
# think on:
#   shell?
#   tensorboard?
#     (integration w/ tensorflow::tensorboard()?. guild does do some convenient
#      prep of the log dir. No daemon managment tho. Also, launchable from guild_view())
#   tensorflow inspect?
#
#
# TODO: impl write_run_attrs(). write out seed, pkg env, etc in do_guild_run()
# TODO: revisit sourcecode selection rules;
#   manually resolve and return a full list in op data?
