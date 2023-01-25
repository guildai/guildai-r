

do_guild_run <-
function(file = "train.R", flags_dest = file, echo = TRUE,
         prune_on_success = TRUE) {

  if (is_r_file(flags_dest)) {
    modify_r_file_flags(flags_dest, read_yaml(".guild/attrs/flags"),
                        overwrite = TRUE)
  } else if (is_yml_file(flags_dest)) {
    file.copy(".guild/attrs/flags", flags_dest, overwrite = TRUE)
  }

  # setup default plot device.
  # the default viewers work better w/ pngs than pdf.
  options(
    "device" = function() {
      plots_dir <- file.path(Sys.getenv("RUN_DIR", "."), "plots")
      if (!dir.exists(plots_dir))
        dir.create(plots_dir, recursive = TRUE)

      # if (is_windows() && capabilities("cairo"))  # required to prevent empty plot
      #   png_args$type <- "cairo"                  # emitted when type = "windows"
      grDevices::png(
        file.path(plots_dir, "Rplot%03d.png"),
        width = 1200, height = 715, res = 192 # ~ golden ratio @ highdpi
      )
    },

    # bump up default so recorded run scalars preserve more precision
    digits = max(15, getOption("digits"))
  )


  seed <- tryCatch({
    as.integer(readLines(".guild/attrs/random_seed"))
  }, warning = function(e) {
    # random_seed probably overflowed, doesn't fit in int32.

    set.seed(NULL) # non-interactive sessions lazily initialize seed.
    seed <- sample.int(.Machine$integer.max, 1L) * sample(c(-1L, 1L), 1L)
    # Garrett to patch core so `random_seed` attr fits in  int32.
    # for now we generate or own and rewrite the attr
    write_run_attr("random_seed", seed)
    seed
  })
  set.seed(seed)

  write_run_attr("env", Sys.getenv())

  for (pkgname in setdiff(loadedNamespaces(), "base"))
    write_run_attr_pkg_loaded(pkgname)

  for (pkgname in installed.packages2())
    setHook(packageEvent(pkgname, "onLoad"), write_run_attr_pkg_loaded)

  source <- new_source_w_active_echo()
  formals(source) <- utils::modifyList(formals(source), list(
    spaced = FALSE,
    keep.source = TRUE,
    deparseCtrl = c("keepInteger", "showAttributes", "keepNA"),
    max.deparse.length = Inf
  ), keep.null = TRUE)
  withCallingHandlers({

    source(file = file, echo = echo)

  },

  error = function(e) {
    # capture error as run attr
    calls <- sys.calls()
    # drop some noise from the call stack and reverse it for nicer printing.
    # First two are: do_guild_run(), withCallingHandlers()
    # Last two are this error handler: .handleSimpleError(), simpleError()
    calls <- calls[(length(calls) - 2L):3L]
    write_run_attr("r_error", list(
      message = e$message,
      call = deparse1(e$call),
      traceback = vapply(calls, deparse1, "", collapse = "\n",
                         width.cutoff = 100L)
    ))

    # re-raise error
    stop(e)
  })

  invisible()
}

write_run_attr_pkg_loaded <- function(pkgname, pkgpath = NULL) {
  ns <- getNamespace(pkgname)
  if(is.null(pkgpath))
    pkgpath <- getNamespaceInfo(ns, "path")

  val <- list(list(path = pkgpath,
                   version = getNamespaceVersion(ns)))
  names(val) <- pkgname
  # TODO: consider not appending to existing attr
  # instead do a full read -> modify -> write each invocation
  write_run_attr("r_packages_loaded", val, append = TRUE)
}


#' @importFrom utils getParseData getSrcLocation
modify_r_file_flags <- function(filename, flags, overwrite = FALSE,
                                text = readLines(filename)) {

  if (!length(text))
    text <- ""

  exprs <- parse(text = text, keep.source = TRUE)
  parse_data <- getParseData(exprs)
  last_line_modified <- 0L

  .replace_token <- function(token_parse_data, new_literal_val) {
    text <<- replace_token(text, token_parse_data, new_literal_val)
    last_line_modified <<- token_parse_data$line2
    invisible()
  }


  for (i in seq_along(exprs)) {
    if (!length(flags))
      break

    # e == expression(); l == lang (call, sym, literal, etc)
    l <- exprs[[i]]

    if (!is.call(l))
      next

    op <- l[[1L]]
    if (op != quote(`=`) && op != quote(`<-`))
      next

    if (typeof(l[[2L]]) != "symbol")
      next

    name <- as.character(l[[2L]])
    if (!name %in% names(flags))
      next

    # "+1", "-1", "1+1i", etc. all get parsed as calls. Eval to resolve the val.
    if(is.call(l[[3L]]) && all(all.names(l[[3]]) %in% c("+", "-")))
      l[[3L]] <- eval(l[[3L]], baseenv())


    if(is_complex_literal(l[[3L]]) &&
       typeof(flags[[name]]) != "complex") {
      # yaml/guild doesn't have a 'complex' flag type. Resolve the correct
      # R val for the flag type as well if needed.
      flags[[name]] <- as.complex(eval(str2lang(flags[[name]]), baseenv()))
        # eval(str2lang()) first because as.complex("1i") fails, returns NA
    }

    if (identical(flags[[name]], l[[3L]])) {
      # new value same as default
      flags[[name]] <- NULL
      next
    }

    # we've found an expression we want to modify.

    # `l` is an assignment call with a flag symbol on
    # the left hand side and a literal on the right hand side.

    # Now we need to update the source text
    # first get precise location of the expression in the source
    # [.expression will slice out the srcref specific to this expression
    e <- exprs[i]

    # if the user defined multiple globals on one line,
    # parse_data${col1,col2} might be incorrect.
    # Just reparse the full text in its partially modified current state.
    # This is relatively expensive but should only happen rarely.
    if(last_line_modified >= getSrcLocation(e, "line")) {
      exprs <- parse(text = text, keep.source = TRUE)
      parse_data <- getParseData(exprs)
      e <- exprs[i]
    }

    line_start <- getSrcLocation(e, "line", first = TRUE)
    line_end   <- getSrcLocation(e, "line", first = FALSE)
    col_start  <- getSrcLocation(e, "column", first = TRUE)
    col_end    <- getSrcLocation(e, "column", first = FALSE)

    if(is.complex(flags[[name]])) {
      ## complex numbers are parsed as two NUM_CONST tokens and a '+' token,
      ## so we special case them. (they may be split across multiple lines)

      # slice out the lines w /this expression
      df <- parse_data
      df <- df[df$line2 <= line_end, ]
      df <- df[df$line1 >= line_start, ]

      # slice out just the two numeric constants
      df <- df[df$token == "NUM_CONST", ]
      stopifnot(nrow(df) %in% c(1L, 2L))

      # TODO: cleanup complex flags injection
      ## either user wrote "1i", "-1i", "1+1i" "-1+1i"
      ## df <- df[df$token %in% c("NUM_CONST", "'+'", "'-'"), ]
      ## stopifnot(nrow(df) %in% c(1L, 2L, 3L, 4L))

      if (length(unique(c(df$line1, df$line2))) == 1) {
        # both NUM_CONSTS of the complex are on the same line.
        # just collapse the parse_data rows and treat as one pseudo token
        # (user whitespace around + is lost, but whatever)
        token <- data.frame(
          line1 = first(df$line1), col1 = first(df$col1),
          line2 =  last(df$line2), col2 =  last(df$col2))
        .replace_token(token, flags[[name]])

      } else {
        # the NUM_CONSTS are on separate lines, potentially with
        # user meaningful whitespace and comments around or in between.
        re_token <- df[1, ]
        im_token <- df[2, ]

        .replace_token(re_token, Re(flags[[name]]))
        .replace_token(im_token, complex(imaginary = Im(flags[[name]])))
      }
    } else {
      # just a regular, non-complex, literal token
      # find the CONST token that ends on the same position as our expression
      # One edge case is negative number literals, like complex literals,
      # are actually parsed as calls.
      df <- parse_data
      df <- df[df$line2 == line_end, ]
      df <- df[df$col2 <= col_end, ]
      df <- df[df$col1 >= col_start, ]
      if (sum(df$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN")) >= 1) {
        stopifnot(sum(df$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN")) == 1)
        df <- df[-(seq(which(df$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN")))),]
      }
      df <- df[df$token != "expr",]
      stopifnot(nrow(df) %in%  c(1, 2))

      if (nrow(df) == 2) {
        stopifnot(df$token[1] %in% c("'+'", "'-'"),
                  df$token[2] == "NUM_CONST")
        sign <- if (flags[[name]] >= 0) quote(`+`) else quote(`-`)
        .replace_token(df[1, ], sign)
        .replace_token(df[2, ], abs(flags[[name]]))
      } else {
        stopifnot(df$token %in% c("NUM_CONST", "STR_CONST", "NULL_CONST"))
        .replace_token(df, flags[[name]])
      }
    }


    if(Sys.getenv("GUILD_DEBUG_R") == 1) {
      # emit message about the magic we just did
      old_l <- l
      l[[3L]] <- flags[[name]] # replace the value in node
      message(sprintf("Replaced expression '%s' on line %i with '%s'",
                      deparse1(old_l), line_start, deparse1(l)))
    }

    flags[[name]] <- NULL # null out flag value; each flag is injected only once

  }

  if(length(flags) && Sys.getenv("GUILD_DEBUG_R") == 1)
    message("Additional flags not injected: \n",
            paste("  ", encode_yaml(flags), collapse = "\n"))

  if(isTRUE(overwrite))
    writeLines(text, filename)

  invisible(text)
}

## this new replace_token() approach introduces two new limitations to the
## `flags-dest: globals` interface.
## 1. expression must be a left assign
## 2. simple math expressions like 2^8 no longer allowed, only literals.

is_complex_literal <- function(x) {
  if(is.complex(x)) return(TRUE)

  typeof(x) == "language" &&
    length(x) == 3L &&
    identical(x[[1L]], quote(`+`)) &&
    is.numeric(x[[2L]]) &&
    is.complex(x[[3L]])
}



replace_token <- function(source_full_text, token_parse_data, new_literal_val) {
  # updates `source_full_text`:
  # - splices out the text at the location described by `token_parse_data`
  # - splices in the text of deparsed `new_literal_val` at the cut location.
  # returns the modified source_full_text
  #
  # if the token spans multiple lines, new_string will always
  # occupy just the first line, and the remaining lines will be "".
  # The total length of lines in the returned `full_text` is guaranteed to be unchanged.
  stopifnot(
    nrow(token_parse_data) == 1,
    c("line1", "col1", "line2", "col2") %in% names(token_parse_data)
  )
  nlines_in <- length(source_full_text)

  new_string <- deparse1(new_literal_val)
  if(is.complex(new_literal_val))
    new_string <- str_drop_prefix(new_string, "0+")

  tk <- token_parse_data
  lines <- source_full_text[tk$line1:tk$line2]

  pre <-  substring(first(lines), 0L, tk$col1 - 1L)
  post <- substring(last(lines), tk$col2 + 1L, .Machine$integer.max)

  stopifnot(length(new_string) == 1L)

  x <- paste0(pre, new_string, post)
  if(length(lines) > 1L)
    x <- c(x, character(length(lines) - 1L))

  source_full_text[tk$line1:tk$line2] <- x

  # guarantee
  stopifnot(nlines_in == length(source_full_text))

  source_full_text
}




#' Is code executing in the context of a guild run?
#'
#' @return Boolean
#' @export
is_run_active <- function()
  !is.na(Sys.getenv("RUN_DIR", NA_character_))



#' Write run attributes
#'
#' This function does nothing if `is_run_active()` is `FALSE`.
#'
#' @param name A string
#' @param data The data to write. This needs to be encodable as yaml.
#'   If missing, the existing attr is deleted.
#' @param append whether to append to the existing attr file.
#' @inheritDotParams yaml::as.yaml -x
#'
#' @keywords internal
#' @return the written lines, invisibly.
write_run_attr <- function(name, data, ..., append = FALSE) {
  if(!is_run_active())
    return(invisible())
  stopifnot(nzchar(name))
  file <- file.path(Sys.getenv("RUN_DIR"), ".guild/attrs", name)
  if(missing(data))
    return(unlink(file))
  data <- encode_yaml(data, ...)
  cat(data, file = file, sep = "\n", append = append)
  invisible(data)
}
