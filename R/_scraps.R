

#' @importFrom xfun strict_list as_strict_list
merge_guild_data <- function(orig, new) {
  stopifnot(is_named_list(orig), is_named_list(new))
  # merge_keys <- c("sourcecode")
  out <- orig

  if(hasName(new, "sourcecode") -> nv) {
    if(is.null(names(nv)))
      nv <- yaml(select = yaml(include = nv))
    else if(!is.null(nv[["select"]] -> sv)) {
      if(is.null(names(sv))) {

        sv <- list("include" = sv)
      }
    }
    out$sourcecode$select <- c(out$sourcecode$select, )
    out[["sourcecode"]] <- c()
  }
  orig[["sourcecode"]] <-

    for(nm in names(new)) {
      nv <- new[[nm]]
      if(is_unnamed_list(ov <- orig[[nm]]))
        orig[[nm]] <- c(ov, nv)
      else
        orig[[nm]] <- nv
    }
  orig
}



# as_sourcecode_spec <- function(x) {
#   if(is.null(x))
#     return(x)
#
#   if(is.character(x))
#     return(yaml(select = yaml(include = x)))
#
#   if(!is.null(sourcecode$selec))
#
# }


# defaults
# data <- list(
#   name = r_script_path,
#   exec = sprintf(
#     r"(Rscript -e 'guildai:::do_guild_run("%s")' ${flag_args})",
#     r_script_path),
#   `flags-dest` = "globals",
#   sourcecode = list(
#     dest = ".",
#     select = list(
#       list(exclude = list(dir = "renv")),
#       list(include = list(text = list(
#         "renv.lock", ".Rprofile", ".Renviron", "*.[rR]")))
#     )))

# data <<- modifyList(data, x)


# "flags-dest": 'globals',
# "flags": script_data['global-flags'],
# "output-scalars": self.output_scalars,
# "objective": self.objective,
# "plugins": self.plugins,
