

is_named_list <- function(x) {
  is.list(x) && !is.null(names(x))
}

is_unnamed_list <- function(x) {
  is.list(x) && is.null(names(x))
}

is_scalar <- function(x) identical(length(x), 1L)

is_hashpipe <- function(x) startsWith(x, "#|")

is_r_file <- function(x) {
  identical(tolower(tools::file_ext(x)), "r")
}

is_yml_file <- function(x) {
  ext <- tools::file_ext(x)
  identical(ext, "yml") || identical(ext, "yaml")
}

is_windows <- function() .Platform$OS.type == "windows"


str_drop_prefix <- function(x, prefix) {
  if (is_string(prefix))
    prefix <- ifelse(startsWith(x, prefix), nchar(prefix), 0L)

  substr(x, as.integer(prefix) + 1L, nchar(x))
}

str_drop_suffix <- function(x, suffix) {
  end <- nchar(x)
  if (is_string(suffix))
    end <- ifelse(endsWith(x, suffix), end - nchar(suffix), end)

  substr(x, 1, end)
}


first <- function(x) x[1L]
last <- function(x) x[length(x)]

`subtract<-` <- function(x, value) x - value
`append<-` <- function(x, value) c(x, value)
`prepend<-` <- function(x, value) c(value, x)



parse_yaml_anno <- function(x) {
  stopifnot(startsWith(x, "#|"))
  x <- substr(x, 4L, .Machine$integer.max)
  x <- parse_yaml(x)
  x
}


new_source_w_active_echo <- function() {
  # returns a modified version of `base::source()` that respects
  # 'options(echo=)' values being changed mid-run.
  # R CMD check complains if a copy of base::source lives in the
  # namespace because of forbidden .Internal() calls, so we
  # have to do this patch at runtime.
  source2 <- base::source
  body(source2) <- substitute({
    orig_echo <- options(echo = echo)
    rm(echo)
    makeActiveBinding("echo", function(x)
      if (missing(x)) getOption("echo") else options(echo = x),
      environment())
    out <- SOURCE_BODY
    options(orig_echo) # can't use on.exit() because base::source() uses it.
    invisible(out)
  }, env = list(SOURCE_BODY = body(source)))
  source2
}


system2t <- function (command, args, ...,
                      echo_cmd = Sys.getenv("GUILD_DEBUG_R") == "1") {
  if(echo_cmd) {
    # cl <- as.call(c(list(quote(system2t), command, args, ...)))
    # message(paste("R>", deparse1(cl)))
    message(paste("Running:", shQuote(command), paste0(args, collapse = " ")))
  }
  system2(command, args, ...)
}

if(getRversion() < "4")
  deparse1 <- function (expr, collapse = " ", width.cutoff = 500L, ...)
    paste(deparse(expr, width.cutoff, ...), collapse = collapse)

if(getRversion() < "3.6.0")
str2lang <- function (s) {
  stopifnot(length(s) == 1L)
  ex <- parse(text = s, keep.source = FALSE)
  stopifnot(length(ex) == 1L)
  ex[[1L]]
}


replace_val <- function(x, old, new) {
  if(!is_scalar(new))
    stop("Unexpected length of replacement value in replace_val().\n",
         "`new` must be length 1, not ", length(new))
  x[x %in% old] <- new
  x
}


installed.packages2 <- function() {
  # faster version that doesn't attempt to read DESCRIPTIONS,
  # only returns folder names or what are ostensibly packages
  unique(unlist(lapply(.libPaths(), list.files)))
}


.globals <- new.env(parent = emptyenv())

find_guild_home <- function() {
  ghome <- Sys.getenv("GUILD_HOME", NA_character_)
  if(!is.na(ghome))
    return(ghome)

  dir <- getwd()
  repeat {
    ghome <- file.path(dir, ".guild")
    if(dir.exists(ghome) ||
       dir == path.expand("~") || # HOME/.guild
       dirname(dir) %in% c(dir, ".")) # filesystem root: e.g. /.guild or C:/.guild)
      return(ghome)
    dir <- dirname(dir)
  }
}

