

do_guild_run <-
function(file = "train.R",
         flags_dest = "globals",
         echo = TRUE,
         flags = parse_command_line(commandArgs(TRUE))) {

  if(flags_dest == "globals")
    text <- update_source_w_global_flags(file, flags, overwrite = FALSE)
  else
    text <- readLines(file)

  source2(
    exprs = parse(text = text, keep.source = TRUE),
    echo = echo,
    spaced = TRUE,
    max.deparse.length = Inf,
    deparseCtrl = c("keepInteger", "showAttributes", "keepNA")
  )

  invisible()
}

source2 <- source
body(source2) <- substitute({
  options(echo = echo)
  rm(echo)
  makeActiveBinding("echo", function(x) {
    if (missing(x)) getOption("echo")
    else options(echo = x)
  }, environment())
  SOURCE_BODY
}, env = list(SOURCE_BODY = body(source)))



#' @importFrom utils getParseData getSrcLocation
update_source_w_global_flags <- function(filename, flags, overwrite = FALSE,
                                         text = readLines(filename)) {

  if (!length(text))
    text <- ""

  exprs <- parse(text = text, keep.source = TRUE)
  parse_data <- getParseData(exprs)
  last_line_modified <- 0L

  .replace_token <- function(token_parse_data, ...) {
    text <<- replace_token(text, token_parse_data, ...)
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

    # workaround guild's accommodating python's odd behavior that requires
    # guild to pass '' or "1" for bool cmd line flags.
    if(typeof(l[[3L]]) == "logical" && !typeof(flags[[name]]) == "logical")  {
      # python's `bool()` only returns False on an empty string '', so that's
      # what guild gives it. Guild arbitrarily gives "1" for True.
      # Our yaml parser converts an empty string to NULL, 1 to an int.
      if(is.null(flags[[name]]))
        flags[[name]] <- FALSE
      else if(identical(flags[[name]], 1L))
        flags[[name]] <- TRUE
    }

    # yaml::yaml.load()/parse_command_line() leaves complex literals as strings
    # meanwhile, `parse()` returns complex literals as `+` calls
    # resolve the actual complex value for both
    if(is_complex_literal(l[[3L]]) &&
       typeof(flags[[name]]) != "complex") {
      l[[3L]] <- eval(l[[3L]], baseenv())
      flags[[name]] <-
        as.complex(eval(str2lang(flags[[name]]), baseenv()))
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

    # if the user defined multiple globals one one line,
    # parse_data$col1/col2 might be incorrect.
    # Just reparse the full text in its partially modified current state.
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
      ## Otherwise, all other flags are scalar literals always parsed as one token

      # slice out the lines w /this expression
      df <- parse_data
      df <- df[df$line2 <= line_end, ]
      df <- df[df$line1 >= line_start, ]

      # slice out just the two numeric constants
      df <- df[df$token == "NUM_CONST", ]
      # either user wrote "1+1i" or "1i"
      stopifnot(nrow(df) %in% c(1L, 2L))

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
      df <- parse_data
      df <- df[df$line2 == line_end, ]
      df <- df[df$col2 == col_end, ]
      df <- df[df$token %in% c("NUM_CONST", "STR_CONST", "NULL_CONST"), ]
      stopifnot(nrow(df) == 1)
      token <- df
      .replace_token(token, flags[[name]])
    }

    # TODO: support for injecting flags for expressions like `foo <- get_foo()`?
    # TODO: ensure/force type-stable flag injections? NULLable flag values?

    # emit message about the magic we just did
    old_l <- l
    l[[3L]] <- flags[[name]] # replace the value in node
    message(sprintf("Replaced expression '%s' on line %i with '%s'",
                    deparse1(old_l), line_start, deparse1(l)))

    flags[[name]] <- NULL # null out flag value; each flag is injected only once

  }

  if(length(flags))
    warning("Unused params received but not injected: ", unlist(flags))

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
    identical(x[[1L]], quote(`+`)) &&
    is.numeric(x[[2L]]) &&
    is.complex(x[[3L]])
}


first <- function(x) x[1L]
last <- function(x) x[length(x)]


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
