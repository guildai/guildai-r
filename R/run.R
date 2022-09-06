


do_guild_run <-
  function(file = "train.R",
           params = parse_command_line(commandArgs(TRUE))) {
    exprs <- parse(file, keep.source = TRUE)
    exprs <- inject_global_param_values(exprs, params)
    withAutoprint(exprs = exprs, evaluated = TRUE, local = .GlobalEnv)
    invisible()
  }


inject_global_param_values <- function(exprs, params) {
  if (!length(params))
    return(exprs)

  for (i in seq_along(exprs)) {
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

    if (!length(params))
      break
  }

  if(length(params))
    warning("Unused params received but not injected: ", unlist(params))

  exprs
}

