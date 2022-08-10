
#
#!/usr/bin/env -S Rscript --vanilla --default-packages=base,guild.ai

infer_and_emit_global_flags <- function(r_script_path) {

  # message(getwd())
  exprs <- parse(r_script_path, keep.source = FALSE)

  flags <- list()
  for(e in exprs) {
    if(!is.call(e))
      next

    op <- e[[1L]]
    if(op != quote(`=`) && op != quote(`<-`))
      next

    if(typeof(e[[2L]]) != "symbol")
      next

    name <- as.character(e[[2L]])

    if(is.call(e[[3L]])) {
      # allow simple exprs of basic fns and literals
      syms <- all.names(e[[3L]])
      if(!all(syms %in% c("list", "c", "as.integer", "as.raw", "[")))
        next
    }

    default <- eval(e[[3L]], envir = .GlobalEnv)
    stopifnot(
      typeof(default) %in%
        c("double", "integer", "character", "logical", "complex"))

    flags[[name]] <- default

  }

  cat(unclass(jsonlite::toJSON(flags, auto_unbox = TRUE, digits = NA)))

  invisible()
}
