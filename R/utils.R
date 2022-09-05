

is_named_list <- function(x) {
  is.list(x) && !is.null(names(x))
}

is_unnamed_list <- function(x) {
  is.list(x) && is.null(names(x))
}


is_hashpipe <- function(x) startsWith(x, "#|")
