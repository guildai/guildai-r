

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
  x <- as.list(x)
  class(x) <- "yaml"
  x
}

#' @export
yaml <- function(...)
  as_yaml(rlang::dots_list(..., .named = TRUE))

maybe_as_yaml <- function(x) {
  if (is.null(x))
    return(NULL)

  if(length(x) > 1L)
    x <- as.list(x)
  if(is.list(x))
    class(x) <- "yaml"
  x
}

#' @export
`$.yaml` <- function(x, name, ...) {
  # no partial matching, preserve 'yaml' class on sublists
  maybe_as_yaml(unclass(x)[[name, ...]])
}

#' @export
`[[.yaml` <- function(x, ...) {
  maybe_as_yaml(NextMethod())
}

#' @export
`[.yaml` <- `[[.yaml`

#' @exportS3Method
str.yaml <- function(x, ...) {
  cat("YAML ")
  str(unclass(x), ...)
}

# registerS3method("print", "yaml", print.yaml)
# registerS3method("$", "yaml", `$.yaml`)
# registerS3method("[[", "yaml", `[[.yaml`)
# registerS3method("[", "yaml", `[.yaml`)
