
#' @importFrom here here
#' @importFrom rlang is_string
guild <- function(command = NULL, ...,
                  stdout = "", stderr = "",
                  env = NULL,
                  wait = TRUE) {

  args <- as_guild_args(I(command %||% character()), ...)

  if(is.na(Sys.getenv("GUILD_HOME", NA_character_)))
    args <- c("-H", shQuote(here(".guild")), args)
  if(Sys.getenv("DEBUG") == "1")
    args <- c("-D", "5678", args)
  system2t(find_guild(), args,
           env = env,
           stdout = stdout, stderr = stderr,
           wait = wait)
}


# as_guild_args(tag = c("a a" , "b", "c c"))
# "--tag" "'a a'" "--tag" "b"     "--tag" "'c c'"
#
#  as_guild_args(help = TRUE)
#  "--help"
as_guild_args <- function(...) {
  args <- .process_args(list(...))
  # protect from shell quoting multiple times if args
  # channel through `as_guild_args()` multiple times
  if(length(args))
    class(args) <- "AsIs"
  args
}



#' @importFrom rlang names2
.process_args <- function(x, name = "") {
  if(is.list(x)) # recurse
    return(unlist(.mapply(
      .process_args,
      list(x, names(x) %||% ""),
      NULL
    ), use.names = FALSE))


  if(is.null(x))
    return(x)

  if(inherits(x, "AsIs") && is.null(names(x)))
    # early return for args that go through
    # as_guild_args() multiple times
    return(x)

  # Fix up names by translating R conventions to shell conventions:
  #   - separate words with - instead of _
  #   - prefix with "--" as needed
  #   - accept a "." prefix as an alias for a "-" prefix
  nms <- if(is.null(names(x))) rep(name, length(x)) else names2(x)
  nms <- sub("^\\.\\.", "--", nms)
  nms <- sub("^\\.", "-", nms)
  needs_prefix <- nzchar(nms) & !startsWith(nms, "-")
  nms[needs_prefix] <- paste0("--", nms[needs_prefix])
  nms <- gsub("_", "-", nms, fixed = TRUE)

  # boolean values are assumed to be switches in the cli
  if(isTRUE(x) && nzchar(nms))
    return(nms)
  if(isFALSE(x) && nzchar(nms))
    return(NULL)

  # cast to char, but preserve class and names
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
  x <- as.list(x)
  for (i in seq_along(nms))
    if (isTRUE(nzchar(nm <- nms[[i]])))
      x[i] <- list(c(nm, x[[i]]))

  unlist(x, use.names = FALSE)
}




# TODO: support for globals injection of `!expr foo` flags

#' Launch a guild run
#'
#' @param opspec typically path to an R script, but could be an scalar
#'   string that guild recognizes as a valid opspec.
#' @param flags flag values for the run(s)
#'
#'   - a named list or vector like `c(noise = .3, dropout = .4)`. Lists with
#'   vectors of length greater than 1 are automatically expanded into a grid
#'   of combinations for a batch of runs. For example, `list(noise = c(.2,
#'   .3), dropout = c(.4, .5)` expands to a batch of 4 runs.
#'
#'   - a dataframe of flags for a batch of runs.
#'
#'   - a scalar string like `"noise=.3 dropout=.4"`. Any flags specification
#'   accepted at the terminal is valid here as well.
#'
#' @param echo whether output from the run is shown in the current R
#'   console. Note, this has no effect on whether expressions are echoed in
#'   the guild run stdout log. To disable echoing of expression in the run
#'   logs, specify `#| echo: false` in the run script frontmatter.
#' @param ... passed on to `guild run`
#' @inheritDotParams guild_run_cli
#'
#' @return the return value from `system2()`, invisibly. This function is
#'   primarily called for its side effect.
#' @export
guild_run <- function(opspec = "train.R",
                      flags = NULL, ...,
                      echo = TRUE) {

  if (is.data.frame(flags)) {
    fi <- tempfile("guild-batch-flags-", fileext = ".yml")
    on.exit(unlink(fi))
    print.yaml(flags, fi, column.major = FALSE)
    flags <- paste0("@", fi)
  } else if (!is.null(names(flags))) {
    # A scalar string for flags is passed through
    # otherwise, do some prep to build the command line arg
    flags <- lapply(flags, function(f) {
      x <- vapply(f, function(fv)
        if (is.character(fv)) fv else encode_yaml(fv),
        "", USE.NAMES = FALSE)
      if (length(x) > 1)
        x <- sprintf("[%s]", paste0(x, collapse = ","))
      x
    })
    flags <- sprintf("%s=%s", names(flags), unname(flags))
  }

  guild("run --yes", list(...), opspec, flags,
        stdout = if(isTRUE(echo)) "" else FALSE)
}


#' Launch Guild Viewer
#'
#' @param runs an optional runs selection.
#' @inheritDotParams guild_view_cli
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
  guild("view", guild_view_opts(...), maybe_extract_run_ids(runs), wait = wait)
}



#' Copy run files into the current project working directory
#'
#' @param run a run selection
#' @inheritDotParams guild_merge_cli
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
  guild("merge", guild_merge_opts(...), maybe_extract_run_ids(run))
}

# dummy place holder, because R CMD check otherwise complains:
#   Namespace in Imports field not imported from: ‘here’
#     All declared Imports should be used.
if(FALSE) {
  here::here()
}


# TODO: other subcommands to wrap + export:
# next:
#   download, publish, api merge
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
# TODO: revisit sourcecode selection rules;
#   manually resolve and return a full list in op data?


## TODO: vectorize guild_run() on script, so can do `guild_run(c("patha.R`, "pathb.R"))`
