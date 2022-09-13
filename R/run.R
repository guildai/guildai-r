

# TODO: rename throughout s/flag/param/g

do_guild_run <-
function(file = "train.R",
         flags_dest = "globals",
         echo = TRUE,
         flags = parse_command_line(commandArgs(TRUE))) {

  run_dir <- getwd()
  # setup_info <- setup_run_dir()

  exprs <- parse(file, keep.source = TRUE)
  if(flags_dest == "globals") # globalenv .Globalenv
    exprs <- inject_global_param_values(exprs, flags)

  # TODO: if flags_dest == "config:flags.yml", guild is not
  # placing an updated "flags.yml" file in the run directory
  # https://rstudio.slack.com/archives/C0366T2TBAL/p1662740546338429.

  # register_magic_hooks()
  options(guildai.is_run_active = TRUE,
          warn = 1L)

  on.exit({
    options(guildai.is_run_active = NULL)
    setwd(run_dir)
    # teardown_run_dir(setup_info)
  })

  source(
    exprs = exprs,
    echo = echo,
    max.deparse.length = Inf,
    deparseCtrl = c("keepInteger", "showAttributes", "keepNA")
  )

  invisible()
}

#' @export
is_run_active <- function() getOption(guiltai.is_run_active, FALSE)


inject_global_param_values <- function(exprs, params) {


  for (i in seq_along(exprs)) {
    if (!length(params))
      return(exprs)

    e <- exprs[[i]]

    if (!is.call(e))
      next

    op <- e[[1L]]
    if (op != quote(`=`) && op != quote(`<-`))
      next

    if (typeof(e[[2L]]) != "symbol")
      next

    name <- as.character(e[[2L]])
    if (!name %in% names(params))
      next

    if(identical(params[[name]], e[[3L]])) { # new value same as default
      params[[name]] <- NULL
      next
    }

    old_e <- e

    # `e` is an assignment expression with a flag symbol on
    # the left hand side
    e[[3L]] <- params[[name]] # replace the value in node
    exprs[[i]] <- e           # update exprs w/ new node
    params[[name]] <- NULL    # null out flag value; each flag is injected only once

    message(sprintf("Replacing expression '%s' on line %i with '%s'",
                    deparse1(old_e), utils::getSrcLocation(exprs[i], "line"),
                    deparse1(e)))

  }

  if(length(params))
    warning("Unused params received but not injected: ", unlist(params))

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
  copied_files <- copied_files[file.exists(rownames(copied_files)), ]
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
