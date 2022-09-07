

#' @export
print.yaml <- function(x, file = "") {
  out <- yaml::as.yaml(
    x, precision = 16L,
    indent.mapping.sequence = FALSE)
  for(f in file)
    cat(out, file = f)
  invisible(out)
}


#' @export
read_yaml <- function(...) {
  x <- yaml::read_yaml(...)
  class(x) <- "yaml"
  x
}

parse_yaml <-  function(...) {
  x <- yaml::yaml.load(...)
  class(x) <- "yaml"
  x
}

#' @export
as_yaml <- function(x) {
  if(is.null(x)) return(x)
  class(x) <- "yaml"
  x
}

#' @export
yaml <- function(...)
  as_yaml(rlang::dots_list(..., .named = TRUE))



#' @export
`$.yaml` <- function(x, name, ...) {
  # no partial matching, preserve 'yaml' class
  out <- unclass(x)[[name, ...]]
  if (is.null(out)) NULL
  else structure(out, class = "yaml")
}

#' @export
`[[.yaml` <- function(x, ...) {
  out <- NextMethod()
  if(!is.null(out))
    class(out) <- "yaml"
  out
}

#' @export
`[.yaml` <- `[[.yaml`


# registerS3method("print", "yaml", print.yaml)
# registerS3method("$", "yaml", `$.yaml`)
# registerS3method("[[", "yaml", `[[.yaml`)
# registerS3method("[", "yaml", `[.yaml`)
