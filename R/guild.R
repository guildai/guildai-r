
#' @importFrom rlang is_string
guild <- function(command = NULL, ...,
                  stdout = "", stderr = "",
                  env = NULL,
                  wait = TRUE) {

  args <- as_cli_args(I(command %||% character()), ...)

  if (Sys.getenv("GUILD_DEBUG_ATTACH") == "1")
    args <- c("-D", "5678", args)

  exec <- if (wait) system2t else processx::process$new
  res <- exec(find_guild(), args,
              env = env,
              stdout = stdout, stderr = stderr,
              echo_cmd = Sys.getenv("GUILD_DEBUG_R") == "1")

  if (is.integer(res)) {
    # common case of stdout == "" and wait = TRUE,
    # system2() returns process return code.
    # promote process errors to R errors
    if (res != 0L)
      stop(simpleError(paste("guild error code:", res),
                       sys.call(-1L)))
    else
      return(invisible())
  }


  res # processx::process handle, stdout json blob, etc.
}


# as_cli_args(tag = c("a a" , "b", "c c"))
# "--tag" "'a a'" "--tag" "b"     "--tag" "'c c'"
#
#  as_cli_args(help = TRUE)
#  "--help"
as_cli_args <- function(...) {
  args <- .process_args(list(...))
  # protect from shell quoting multiple times if args
  # channel through `as_cli_args()` multiple times
  if(length(args))
    class(args) <- "AsIs"
  args
}



#' @importFrom rlang names2
.process_args <- function(x, name = "") {
  if(is.list(x) && length(x)) # recurse
    return(unlist(.mapply(
      .process_args,
      list(x, names(x) %||% ""),
      NULL
    ), use.names = FALSE))


  if(!length(x))
    return(x)

  if(inherits(x, "AsIs") && is.null(names(x)))
    # early return for args that go through
    # as_cli_args() multiple times
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

  # boolean values are assumed to be negatable switches in the cli
  # so foo=TRUE becomes `--foo`
  # so foo=FALSE becomes `--not-foo`
  # so foo=NA becomes ``
  # use NA_if_FALSE() to treat foo=FALSE as ``
  if (length(x) == 1 && isTRUE(nzchar(nms))) {
    if(is.na(x))
      return(NULL)
    if (isTRUE(x))
      return(nms)
    if (isFALSE(x))
      return(paste0("--not-", str_drop_prefix(nms, "--")))
  }

  # cast to char, but preserve class and names
  storage.mode(x) <- "character"

  # TODO: we shouldn't quote if using  processx::process$new() or processx::run()
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


NA_if_FALSE <- function(x) {
  rapply(list(x), function(e) if(isFALSE(e)) NA else e,
         how = "replace")[[1L]]
}


#' Launch a guild run
#'
#' @param opspec typically a path to an R script, but could be any
#'   string that guild recognizes as a valid operation.
#' @param flags flag values for the run(s).
#'
#'   - A named list or vector like `c(noise = .3, dropout = .4)`. Lists with
#'   vectors of length greater than 1 are automatically expanded into a
#'   grid of combinations for a batch of runs. For example, `list(noise
#'   = c(.2, .3), dropout = c(.4, .5)` expands to a batch of 4 runs.
#'
#'   - A dataframe of flags for a batch of runs, one row per run.
#'
#'   - A scalar string like `"noise=.3 dropout=.4"`. Any flags specification
#'   accepted at the terminal is valid here as well.
#'
#' @param echo whether output from the run is shown in the current R
#'   console. Note, this has no effect on whether expressions are echoed
#'   in the guild run stdout log. To disable echoing of expression in
#'   the run logs, specify `#| echo: false` in the run script
#'   frontmatter.
#'
#' @param as_job Run the operation as an RStudio background job. This is
#'   ignored outside of the RStudio IDE.
#'
#' @param ... passed on to `guild run`
#' @inheritDotParams guild_run_cli
#'
#' @return `NULL`, invisibly. This function is called for its
#'   side effect.
#' @export
guild_run <-
function(opspec = "train.R",
         flags = NULL, ...,
         echo = TRUE,
         as_job = getOption("guildai.run_as_job", TRUE)) {

  if(as_job && rstudioapi::isAvailable()) {
    cl <- as.call(c(quote(guildai::guild_run),
                    as.list.environment(environment()),
                    ...))
    cl$as_job <- FALSE
    script <- tempfile("guildai-rstudio-job-", fileext = ".R")
    writeLines(deparse(cl), script)
    return(invisible(rstudioapi::jobRunScript(
      script, name = paste("guild run", opspec),
      workingDir = getwd())))
  }

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

  if(length(echo) == 1)
    echo <- c(echo, TRUE)

  guild("run", "--yes", list(...), opspec, flags,
        stdout = if(isTRUE(echo[[1L]])) "" else FALSE,
        stderr = if(isTRUE(echo[[2L]])) "" else FALSE)
}


#' Launch Guild Viewer
#'
#' @param runs an optional runs selection.
#' @param ... passed on to `guild view`.
#' @param host Name of host interface to listen on.
#' @param port Port to listen on.
#' @param include_batch (bool) Include batch runs.
#' @param no_open (bool) Don't open Guild View in a browser.
#' @param stop Stop the existing Guild View application.
#'
#' @return The url where the Guild View application can be accessed.
#' @return The url where the View application can be accessed,
#'   invisibly.
#' @export
#' @examples
#' \dontrun{
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
guild_view <- function(runs = NULL,
                       ...,
                       host = NULL,
                       port = NULL,
                       include_batch = FALSE,
                       no_open = FALSE,
                       stop = FALSE) {
  if(!is.null(.globals$view_process)) {
    .globals$view_process$interrupt()
    .globals$view_process$poll_io(500)
    .globals$view_process$kill()
    .globals$view_process <- NULL
    Sys.sleep(0.05)
  }
  if(isTRUE(stop))
    return(invisible())
  .globals$view_process <- p <-
    guild("view", list(
      NA_if_FALSE(mget(c("host", "port", "include_batch", "no_open"))),
      ...),
      maybe_extract_run_ids(runs),
      wait = FALSE, stdout = "|", stderr = "|"
    )
  p$poll_io(1000)
  output <- p$read_output_lines()
  output <- str_drop_suffix(output, " (Type Ctrl-C to quit)")
  message(output)
  url <- sub("Running Guild View at (http.+)", "\\1", output)
  invisible(url)
}



#' Copy run files into the current project working directory
#'
#' @param run a run selection
#' @inheritDotParams guild_merge_cli
#'
#' @export
#' @return `NULL`, invisibly. This function is called for its
#'   side effect.
#'
#' @examples
#' \dontrun{
#' guild_merge("--help")
#' runs_scalars() %>%
#'   dplyr::slice_max("epoch_acc") %>%
#'   guild_merge(I("--yes --replace"))
#' }
guild_merge <- function(run = NULL, ...) {
  guild("merge", guild_merge_cli(...), maybe_extract_run_ids(run))
}
