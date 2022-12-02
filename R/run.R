

## TODO: need to move setup_run_dir() and teardown_run_dir() out of here and
## into guild core run callbacks. Garret objects strongly on philosophical
## grounds, but the workaround required from here is too big w.r.t R session
## initialization. Specifically, sourcing .Rprofile and .Renviron (mechanism
## renv uses, also mechanism commonly used to attach a project-local utils
## env.).
## https://my.guild.ai/t/guild-run-callbacks/925


do_guild_run <-
function(file = "train.R", flags_dest = file, echo = TRUE) {

  if (is_r_file(flags_dest)) {
    modify_r_file_flags(flags_dest, read_yaml(".guild/attrs/flags"),
                        overwrite = TRUE)
  } else if (is_yml_file(flags_dest)) {
    file.copy(".guild/attrs/flags", flags_dest, overwrite = TRUE)
  }

  # setup_info <- setup_run_dir()
  # on.exit(teardown_run_dir(setup_info))



  # setup default plot device.
  # the default viewers work better w/ pngs than pdf.
  options(
    "device" = function() {
      # TODO: after dest.dir change, need to revisit this default plots dir location
      # What happens if a file is overwritten? Should the default plots dir be
      # under .guild/plots perhaps?
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
    digits = max(17, getOption("digits"))
  )

  # initialize seed so we can save it, non-interactive sessions lazily initialize seed.
  set.seed(NULL)
  seed <- sample.int(.Machine$integer.max, 1L) * sample(c(-1L, 1L), 1L)
  set.seed(seed)

  write_run_attr("random_seed", data = ) # delete misleading existing attr
  write_run_attr("r-random-seed", seed)

  write_run_attr("env", Sys.getenv())
  write_run_attr("r-sys-info", Sys.info())

  # on.exit({
  ### waaay too verbose, includes every pkg description
  #   write_run_attr("r-session-info", sessionInfo())
  # })

  for (pkgname in rev(loadedNamespaces())) {
    if(pkgname %in% "base") next
    write_run_attr_pkg_loaded(pkgname)
  }

  for(pkgname in installed.packages2())
    setHook(packageEvent(pkgname, "onLoad"), write_run_attr_pkg_loaded)


  # TODO: figure out goldilocks default for what to record from R session state.
  #   installed.packages() / renv::something() / on.exit(sessionInfo())
  # write_run_attr("r_env")

  source2 <- new_source_w_active_echo()

  withCallingHandlers({

    source2(
      file = file,
      echo = echo,
      spaced = FALSE,
      max.deparse.length = Inf,
      keep.source = TRUE,
      deparseCtrl = c("keepInteger", "showAttributes", "keepNA")
    )

  },

  error = function(e) {
    # capture error as attr
    write_run_attr("r-error", list(
      message = e$message,
      traceback = deparsed_call_stack()
    ))
    # re-raise error
    stop(e)
  })

  invisible()
}

installed.packages2 <- function() {
  # faster version that doesn't attempt to read DESCRIPTIONS,
  # only returns folder names or what are ostensibly packages
  unique(unlist(lapply(.libPaths(), list.files)))
}

write_run_attr_pkg_loaded <- function(pkgname, pkgpath = NULL) {
  ns <- getNamespace(pkgname)
  if(is.null(pkgpath))
    pkgpath <- getNamespaceInfo(ns, "path")

  val <- list(list(path = pkgpath,
                   version = getNamespaceVersion(ns)))
  names(val) <- pkgname
  write_run_attr("r-packages-loaded", val, append = TRUE)
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

    if(is.call(l[[3L]])) {
      cl <- l[[3L]][[1L]]
      if (identical(cl, quote(`+`)) || identical(cl, quote(`-`)) &&
          length(l[[3L]]) == 2L &&
          is.numeric(l[[3L]][[2L]])) {
        # it's a literal numeric with a unary + or - call
        # this is the way negative literals are parsed.
        # eval it so we can do comparisons on the actual val.
        l[[3L]] <- eval(l[[3L]], baseenv())
      }
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
      stopifnot(nrow(df) %in% c(1L, 2L))

      ### TODO:
      ## either user wrote "1i", "-1i", "1+1i" "-1+1i"
      # df <- df[df$token %in% c("NUM_CONST", "'+'", "'-'"), ]
      # stopifnot(nrow(df) %in% c(1L, 2L, 3L, 4L))

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
    length(x) == 3L &&
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



setup_run_dir <- function() {
  # populate run_dir with copies of all the contents of .guild/sourcecode/**
  #
  # Note, the first draft of this experimented with setting up a symlink forest
  # in the run dir of symlinks to .guild/sourcecode, but decided on copies instead
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
  copied_files <- copied_files[file.exists(rownames(copied_files)), ]
  if(!nrow(copied_files)) return(invisible())

  pre_run <- copied_files
  post_run <- file.info(rownames(pre_run))

  # remove anything that we setup.
  not_modified <- pre_run$mtime == post_run$mtime
  if(any(not_modified))
    unlink(rownames(copied_files)[not_modified])

  # prune never-accessed captured sourcecode
  not_accessed <- pre_run$atime == post_run$atime
  if(any(not_accessed))
    file.remove(file.path(
      ".guild/sourcecode", rownames(copied_files)[not_accessed]))

  # while we have it convenient, update access_time for files captured in .guild/sourcecode
  # accessed <- post_run[!not_accessed,]
  # fs::file_touch(rownames(accessed), access_time = accessed$atime)

  # delete directories we created that are empty
  for(d in rev(rownames(setup_info$created_dirs)))
    if(!length(list.files(d, all.files = TRUE, no.. = TRUE)))
      file.remove(d)

  invisible()
}

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
#' @return the written lines, invisibly.
write_run_attr <- function(name, data, ..., append = FALSE) {
  if(!is_run_active())
    return(invisible())
  stopifnot(nzchar(name))
  file <- file.path(Sys.getenv("RUN_DIR"), ".guild/attrs", name)
  if(missing(data))
    return(unlink(file))
  data <- encode_yaml(data, ...)
  cat(data, "", file = file, sep = "\n", append = append)
  invisible(data)
}

# TODO: scalars not detected if they're a flag. Are name collisions between
# flags and scalars not allowed?

deparsed_call_stack <- function(n = 1L) {
  calls <- rev(sys.calls())[-seq_len(n)]
  calls <- vapply(calls, deparse1, "",
                  collapse = "\n", width.cutoff = 100L)
  calls
}


# TODO: delete unread sourcecode files
# TODO: guild run should cast intergerish
#   floats to int if the flag type is int

remove_guild_from_PYTHONPATH <- function() {

  # guild places itself on the PYTHONPATH, which interferes w/ reticulate
  # finding modules. the guild executable is stand-alone and should not
  # leak through into the R op runtime.
  # Values like this are observed on linux:
  # Sys.setenv("PYTHONPATH" = ".guild/sourcecode:/home/tomasz/.local/share/r-guildai/lib/python3.8/site-packages")
  # passing `python-path: ~|null|[]|''` in op data doesn't disable this.

  private_venv_home <- dirname(dirname(find_guild()))
  old_path <- paths <- Sys.getenv("PYTHONPATH")
  paths <- strsplit(paths, split = .Platform$path.sep, fixed = TRUE)[[1]]
  paths <- gsub("\\\\", "/", paths) # windows
  paths <- setdiff(paths, ".guild/sourcecode")
  paths <- paths[!startsWith(paths, private_venv_home)]
  if(!length(paths))
    Sys.unsetenv("PYTHONPATH")
  else {
    if(is_windows())
      paths <- shortPathName(paths)
    Sys.setenv(PYTHONPATH = paste(paths, collapse = .Platform$path.sep))
  }
  invisible(old_path)
}
