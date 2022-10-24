

is_named_list <- function(x) {
  is.list(x) && !is.null(names(x))
}

is_unnamed_list <- function(x) {
  is.list(x) && is.null(names(x))
}


is_hashpipe <- function(x) startsWith(x, "#|")

`subtract<-` <- function(x, value) x - value

`append<-` <- function(x, value) c(x, value)

is_windows <- function ()  .Platform$OS.type == "windows"



new_source_w_active_echo <- function() {
  # R CMD check complains if a copy of base::source lives in the
  # namespace because of forbidden .Internal() calls, so we
  # have to do this patch at runtime.
  source2 <- base::source
  body(source2) <- substitute({
    options(echo = echo)
    rm(echo)
    makeActiveBinding("echo", function(x) {
      if (missing(x)) getOption("echo")
      else options(echo = x)
    }, environment())
    SOURCE_BODY
  }, env = list(SOURCE_BODY = body(source)))
  source2
}


system2t <- function (command, args, ...) {
  if(Sys.getenv("DEBUGR") == "1") {
    cl <- as.call(c(list(quote(system2t), command, args, ...)))
    message(paste("R>", deparse1(cl)))
    message(paste("sys+", shQuote(command), paste0(args, collapse = " ")))
  }
  system2(command, args, ...)
}
