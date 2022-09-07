

is_named_list <- function(x) {
  is.list(x) && !is.null(names(x))
}

is_unnamed_list <- function(x) {
  is.list(x) && is.null(names(x))
}


is_hashpipe <- function(x) startsWith(x, "#|")

`subtract<-` <- function(x, value) x - value

`append<-` <- function(x, value) c(x, value)


# `append<-` <- function(x, value) {
#   stopifnot(identical(mode(x), mode(value)))
#   idx <- seq.int(from = length(x)+1L, along.with = value)
#   x[idx] <- value
#   if(!is.null(names(value)))
#     names(x)[idx] <- names(value)
#   x
# }
