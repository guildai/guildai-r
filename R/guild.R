


guild <- function(...,
                  stdout = "", stderr = "",
                  home = Sys.getenv("GUILD_HOME", here::here(".guild")),
                  wait = TRUE) {

  args <- list(...)
  args <- rapply(args, function(x) {
    if (inherits(x, "AsIs") || all(grepl("^[[:alpha:]_-]+$", x)))
      x
    else
      shQuote(x)
  })
  stopifnot(is.null(names(args)))

  ##? allow args like guild("--path" = r_sym)
  # for(nm in names(args))
  #   if(isTRUE(nzchar(nm)))
  #     args[[nm]] <- sprintf("%s=%s", nm, args[[nm]])

  args <- as.character(unlist(args))

  if(!is.null(home))
    args <- c("-H", shQuote(home), args)
  if(Sys.getenv("DEBUG") == "1")
    args <- c("-D", "5678", args)
  system2t(find_guild(), args,
           stdout = stdout, stderr = stderr,
           wait = wait)
}


.guild <- function(args, ...) {
  # convenience version that accepts args as a single string
  guild(unlist(strsplit(args, "\\s+", perl = TRUE)), ...)
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
guild_run <- function(opspec = "train.R", flags = NULL, ...,
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
#' @param ... passed on to the `guild` binary
#' @param wait whether to block the R console while the application is active.
#'
#' @export
guild_view <- function(..., wait = FALSE) {
  # TODO: use processx here?
  guild("view", ..., wait = wait)
}
