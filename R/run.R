

# TODO: rename throughout s/flag/param/g

do_guild_run <-
function(file = "train.R",
         flags_dest = "globals",
         echo = TRUE,
         flags = parse_command_line(commandArgs(TRUE))) {

  run_dir <- getwd()
  # setup_info <- setup_run_dir()

  if(flags_dest == "globals") # globalenv .Globalenv
    # update_source_w_global_flags(file, flags, overwrite = TRUE)
    text <- update_source_w_global_flags(file, flags, overwrite = FALSE)
  else
    text <- readLines(file)


  exprs <- parse(text = text, keep.source = TRUE)

#
#   exprs <- parse(file, keep.source = TRUE)
#   if(flags_dest == "globals") # globalenv .Globalenv
#     exprs <- inject_global_param_values(exprs, flags)
#   else if (startsWith(flags_dest, "config:")) {
#     # workaround around guild bug
#     # boolean flags don't make it into the yaml file that guild provides
#     # yaml::write_yaml(flags, str_drop_prefix(flags_dest, "config:"))
#   }

  # TODO: if flags_dest == "config:flags.yml", guild is not
  # placing an updated "flags.yml" file in the run directory
  # https://rstudio.slack.com/archives/C0366T2TBAL/p1662740546338429.

  # register_magic_hooks()
  options(warn = 1L)

  on.exit({
    setwd(run_dir)
    # teardown_run_dir(setup_info)
  })


  source(
    # file = file,
    exprs = exprs,
    echo = echo,
    spaced = TRUE,
    max.deparse.length = Inf,
    deparseCtrl = c("keepInteger", "showAttributes", "keepNA")
  )

  invisible()
}

# if(FALSE) {
#   globals <- list()
#   while(nrow(df)) {
#     # df <- df[-(1:which.max(df$token == "SYMBOL"),]
#   }
#   df <- getParseData(exprs)
#   symbols <- df$id[df$token == "SYMBOL"]
#   df$id[df$parent %in% symbols]
#   df[df$id %in% assigned_symobls,]
# }

update_source_w_global_flags <- function(filename, flags, overwrite = FALSE,
                                         text = readLines(filename)) {

  # cat(commandArgs(), sep = "\n")
  # text <- readLines(filename, warn = FALSE) #, encoding = encoding)
  if (!length(text))
    text <- ""
  srcfile <- srcfilecopy(filename, text, file.mtime(filename),
                         isFile = TRUE)
  exprs <- parse(text = text, srcfile = srcfile, keep.source = TRUE)
  srcrefs <- attr(exprs, "srcref", TRUE)
  parse_data <- getParseData(exprs)
  last_line_modified <- 0L

  # .replace_token <- function(...) {
  #   res <- replace_token(text, ...)
  #   text <<- rex$text
  #   parse_data <<- res$parse_data
  #   invisible()
  # }

  .replace_token <- function(token_parse_data, ...) {
    text <<- replace_token(text, token_parse_data, ...)
    last_line_modified <<- token_parse_data$line2
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
      flags[[name]] <- eval(str2lang(flags[[name]])) # as.complex("1i") fails, returns NA
    }

    if(identical(flags[[name]], l[[3L]])) {
      # new value same as default
      flags[[name]] <- NULL
      next
    }

#
#     # we've found an expression we want to modify.

#
#     # `l` is an assignment call with a flag symbol on
#     # the left hand side
#     l[[3L]] <- flags[[name]] # replace the value in node
#     new_l <- deparse1(l)

    # get precise coordinates in the source:
    # Now we need to update it in the source text
    # [.expression will slice out the srcref specific to this expression
    old_e <- exprs[i]
    line_start <- getSrcLocation(old_e, "line", first = TRUE)
    line_end   <- getSrcLocation(old_e, "line", first = FALSE)
    col_start  <- getSrcLocation(old_e, "column", first = TRUE)
    col_end    <- getSrcLocation(old_e, "column", first = FALSE)

    # TODO: this complex handling pathway should move into replace_token()
    # replace_token should take a 2 row df of parse_data
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
      # if(name == "cx2") {
      #   browser()
      #   .replace_token(df, flags[[name]])
      # }
      stopifnot(nrow(df) %in% c(1, 2))
      stopifnot(df$line1 > last_line_modified)
      # check that col1/col2 info is not stale,
      # if user assigned multiple globals on one line separated by ;
      # TODO: handle case of missing real part, '1i' is valid syntax too

      if (length(unique(c(df$line1, df$line2))) == 1) {
        # the complex is two num consts and `+` on one line.
        # just collapse it and treat as one pseudo token
        # (whitespace around + is lost but whatever)
        token <- data.frame(
          line1 = first(df$line1[1]), col1 = first(df$col1),
          line2 = last(df$line2[1]), col2 = last(df$col2))
        .replace_token(token, flags[[name]])

      } else {
        re_token <- df[1,]
        im_token <- df[2,]

        .replace_token(re_token, Re(flags[[name]]))
        .replace_token(im_token, complex(imaginary = Im(flags[[name]]))) # need to add a i prefix
      }
    } else {
      # find the token that ends on the same position as the expression
      df <- parse_data
      df <- df[df$line2 == line_end, ]
      df <- df[df$col2 == col_end, ]
      df <- df[df$token %in% c("NUM_CONST", "STR_CONST", "NULL_CONST"), ]
      stopifnot(nrow(df) == 1)
      # check that col1/col2 info is not stale,
      # if user assigned multiple globals on one line separated by ;
      stopifnot(df$line1 > last_line_modified)
      token <- df
      .replace_token(token, flags[[name]])
    }

    # flags[[name]] <- NULL

    # df <- parse_data[parse_data$line2 = line_end, ]
    # const_parse_data <- df[which.max(df$token %in% c("NUM_CONST", "STR_CONST", "NULL_CONST")),]
    # # replace just the text for the literal
    #
    #
    # const_text_lines <- text[const_parse_data$line1:const_parse_data$line2]
    # pre_const_text <- substring(first(const_text_lines), 0, const_parse_data$col1 - 1)
    # post_const_text <- substring(last(const_text_lines), const_parse_data$col2 + 1,
    #                              .Machine$integer.max)
    # new_const_text <- deparse1(flags[[name]], collapse = "\n", width.cutoff = 80L)
    # new_const_text <- paste0(pre_const_text, new_const_text, post_const_text)
    # new_const_text <- c(new_const_text, character(max(0, length(const_text_lines)-1L)))
    # text[const_parse_data$line1:const_parse_data$line2] <- new_const_text
    #
    # last_line <- const_text_lines[length(const_text_lines)]
#     old_e_lines <- text[line_start:line_end]
#     post_e_text <- substring(last_line, col_end+1, nchar(last_line))
#
#     getParseText()
#
#     browser()


    # browser()
    old_l <- l
    l[[3L]] <- flags[[name]] # replace the value in node
    flags[[name]] <- NULL    # null out flag value; each flag is injected only once

    # exprs[[i]] <- l          # update exprs w/ new node

    # zap out srcref for the expression, so that source(echo=TRUE)
    # actually deparses it
    # srcrefs[i] <- list(NULL)
    # alternative approach: parse(text = c("#line {nn} {filename}", flag_assignment_expr))

    # new_l <- deparse1(l)
    message(sprintf("Replacing expression '%s' on line %i with '%s'",
                    deparse1(old_l), line_start,
                    deparse1(l)))

  }

  if(length(flags))
    warning("Unused params received but not injected: ", unlist(flags))
#
#   attr(exprs, "srcref") <- srcrefs
#   attr(exprs, "wholeSrcref") <- NULL
  # exprs

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


# replace_token <- function(source_full_text, token_parse_data, new_literal_val) {
#   # splices out the text in the given coordinates, splices in `new_string`
#   # at the cut location. if the coordinates span multiple lines, new_string will always
#   # occupy just the first line, and the remaining lines will be "".
#   # The total length of lines in the returned `full_text` is guaranteed to be the same.
#
#   stopifnot(is.data.frame(token_parse_data),
#             hasName(token_parse_data, c("line1", "col1", "line2", "col2")))
#   if (is.complex(new_literal_val)) {
#     stopifnot(nrow(token_parse_data) %in% c(1, 2))
#   } else {
#     stopifnot(nrow(token_parse_data) == 1)
#   }
#
#   nlines_in <- length(source_full_text)
#
#   new_string <- deparse1(new_literal_val)
#   # browser()
#   if(is.complex(new_literal_val)) {
#     new_string <- str_drop_prefix("0+", new_string)
#     new_string <- paste0(new_string, "i")
#     # browser()
#   }
#
#   # a cursor concept would be real nice here...
#   for(r in seq_len(nrow(token_parse_data))) {
#     tk <- token_parse_data[r,]
#
#     lines <- source_full_text[tk$line1:tk$line2]
#
#     pre <-  substring(first(lines), 0L, tk$col1 - 1L)
#     post <- substring(last(lines), tk$col2 + 1L, .Machine$integer.max)
#
#     stopifnot(length(new_string) == 1L)
#
#     x <- paste0(pre, new_string, post)
#     if(length(lines) > 1L)
#       x <- c(x, character(length(lines) - 1L))
#
#     # message(sprintf("Replacing lines '%s' with '%s'",
#     #                 paste(lines, collapse = "\n"), x))
#
#     source_full_text[tk$line1:tk$line2] <- x
#   }
#
#   # guarantee
#   stopifnot(nlines_in == length(source_full_text))
#
#   source_full_text
# }


replace_token <- function(source_full_text, token_parse_data, new_literal_val) {
  # splices out the text in the given coordinates, splices in `new_string`
  # at the cut location. if the coordinates span multiple lines, new_string will always
  # occupy just the first line, and the remaining lines will be "".
  # The total length of lines in the returned `full_text` is guaranteed to be the same.
  stopifnot(
    nrow(token_parse_data) == 1,
    c("line1", "col1", "line2", "col2") %in% names(token_parse_data)
  )
  nlines_in <- length(source_full_text)

  tk <- token_parse_data
  new_string <- deparse1(new_literal_val)
  # browser()
  if(is.complex(new_literal_val)) {
    # browser()
    new_string <- str_drop_prefix(new_string, "0+")
    # new_string <- paste0(new_string, "i")
  }

  lines <- source_full_text[tk$line1:tk$line2]

  pre <-  substring(first(lines), 0L, tk$col1 - 1L)
  post <- substring(last(lines), tk$col2 + 1L, .Machine$integer.max)

  stopifnot(length(new_string) == 1L)

  x <- paste0(pre, new_string, post)
  if(length(lines) > 1L)
    x <- c(x, character(length(lines) - 1L))

  # message(sprintf("Replacing lines '%s' with '%s'",
  #                 paste(lines, collapse = "\n"), x))

  source_full_text[tk$line1:tk$line2] <- x

  # guarantee
  stopifnot(nlines_in == length(source_full_text))

  source_full_text
}





replace_text <- function(full_text, line1, col1, line2, col2, new_string) {
  # splices out the text in the given coordinates, splices in `new_string`
  # at the cut location. if the coordinates span multiple lines, new_string will always
  # occupy just the first line, and the remaining lines will be "".
  # The total length of lines in the returned `full_text` is guaranteed to be the same.
  lines <- full_text[line1:line2]
  pre <-  substring(first(lines), 0L, col1 - 1L)
  post <- substring(last(lines), col2 + 1L, .Machine$integer.max)
  stopifnot(length(new_string) == 1L)
  x <- paste0(pre, new_string, post)
  if(length(lines) > 1L)
    x <- c(x, character(length(lines) - 1L))

  nlines_in <- length(full_text)
  full_text[line1:line2] <- x

  # guarantee
  stopifnot(nlines_in == length(full_text))

  full_text
}



#' Is code executing in the context of a guild run?
#'
#' @return Boolean
#' @export
is_run_active <- function()
  !is.na(Sys.getenv("RUN_DIR", NA_character_))


inject_global_param_values <- function(exprs, flags) {

  srcrefs <- attr(exprs, "srcref", TRUE)

  for (i in seq_along(exprs)) {
    if (!length(flags))
      break

    # e: expression(), l: lang (call, sym, literal, etc)
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

    if(identical(flags[[name]], l[[3L]])) {
      # new value same as default
      flags[[name]] <- NULL
      next
    }

    old_l <- l
    old_e <- exprs[i] # full expression() w/ srcref attr

    # `e` is an assignment expression with a flag symbol on
    # the left hand side
    l[[3L]] <- flags[[name]] # replace the value in node
    exprs[[i]] <- l          # update exprs w/ new node
    flags[[name]] <- NULL    # null out flag value; each flag is injected only once

    # spoof a new srcref, so source(echo = TRUE) gives correct output
    # reparsed <- parse(
    #   text = c(sprintf("#line %i %s",
    #                    getSrcLocation(old_e, "line", TRUE),
    #                    getSrcFilename(old_e)),
    #            deparse1(l))
    # )
    #
    # new_srcref <- attr(reparsed, "srcref")[[1L]]
    # attr(new_srcref, "srcfile") <- attr(srcrefs[[i]], "srcfile")
    # srcrefs[[i]] <- new_srcref

    # ,
    # srcfile = "tests/testthat/resources/flags-from-globals.R")
    # srcfile = attr(srcrefs[[i]], "srcfile"))
    # srcrefs[[i]] <- [[1L]]
    # srcrefs[[i]] <- attr(reparsed, "srcref")[[1L]]

    # zap out srcref for the expression, so that source(echo=TRUE)
    # actually deparses it
    # srcrefs[i] <- list(NULL)
    # alternative approach: parse(text = c("#line {nn} {filename}", flag_assignment_expr))

    message(sprintf("Replacing expression '%s' on line %i with '%s'",
                    deparse1(old_l), utils::getSrcLocation(exprs[i], "line"),
                    deparse1(l)))


  }

  if(length(flags))
    warning("Unused params received but not injected: ", unlist(flags))

  attr(exprs, "srcref") <- srcrefs
  attr(exprs, "wholeSrcref") <- NULL
  exprs
}


setup_run_dir <- function() {
  # populate run_dir with copies of all the contents of .guild/sourcecode/**
  #
  # Note, the first draft of this experimented with setting up a symlink forest
  # in the run dir of symlinks to .guild/sourcecode, but decided on copies inestead
  # because if users attempts to modify a file / writing to a symlink, they'd be
  # modifying the captured sourcode, which should be immutable.
  files <- list.files(".guild/sourcecode", recursive = TRUE, all.files = TRUE)

  # create subdirs as needed
  dirs <- unique(unlist(unique(dirname(files))))
  # sort dirs by depth
  dirs <- dirs[order(lengths(strsplit(dirs, .Platform$file.sep, fixed = TRUE)))]
  dirs <- dirs[!dir.exists(dirs)]
  created_successfully <- as.logical(.mapply(
    function(d, mode) dir.create(d, mode = mode),
    list(dirs, file.info(file.path(".guild/sourcecode", dirs))$mode), NULL))
  created_dirs <- dirs[created_successfully]

  copied_successfully <- file.copy(file.path(".guild/sourcecode", files), files,
                                   overwrite = FALSE, copy.date = TRUE)
  copied_files <- files[copied_successfully]

  # TODO, guild should do this when it setups up the sourcecode folder,
  # mark captured files as immutable (it should probably tarball them too)
  fs::file_chmod(files, "-w")

  list(created_dirs = file.info(created_dirs),
       copied_files = file.info(copied_files))
}

teardown_run_dir <- function(setup_info) {

  stopifnot(is.data.frame(setup_info$copied_files),
            is.data.frame(setup_info$created_dirs))

  copied_files <- setup_info$copied_files

  # filter for existing files, in case user deleted files in run
  copied_files <- copied_files[file.exists(oownames(copied_files)), ]
  if(!nrow(copied_files)) return(invisible())

  pre_run <- copied_files
  post_run <- file.info(rownames(pre_run))

  # remove anything that we setup.
  not_modified <- pre_run$mtime == post_run$mtime
  if(any(not_modified))
    file.remove(rownames(copied_files)[not_modified])

  # prune never-accessed captured sourcecode
  not_accessed <- pre_run$atime == post_run$atime
  if(any(not_accessed))
    file.remove(file.path(
      ".guild/sourcecode", rownames(copied_files)[not_accessed]))

  # while we have it convenient, update access_time for files captured in .guild/sourcecode
  accessed <- post_run[!not_accessed,]
  fs::file_touch(rownames(accessed), access_time = accessed$atime)

  # delete directories we created that are empty
  for(d in rev(rownames(setup_info$created_dirs)))
    if(!length(list.files(d, all.files = TRUE, no.. = TRUE)))
      file.remove(d)

  invisible()
}


register_magic_hooks <- function() {

#   setHook(packageEvent("keras", "onLoad"),
#           function(pkgname, pkgpath) {
#             # patch fit/evaluate to inject tensorboard callback?
#             # modify guild-data mid/post run to change
#             # to `output-scalars: off` if we detect user (or us w/ magic)
#             # are using tensorboard?
#           })

}
