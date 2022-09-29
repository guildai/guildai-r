

# @export
print.yaml <- function(x, file = "", ...) {
  as_yaml_args <- utils::modifyList(list(
    precision = 16L,
    indent.mapping.sequence = FALSE,
    handlers = list(complex = as.character)
  ),
  list(...))
  out <- do.call(yaml::as.yaml, c(list(x), as_yaml_args))
  for (f in file)
    cat(out, file = f)
  invisible(out)
}


# @export
read_yaml <- function(...) {
  maybe_as_yaml(yaml::read_yaml(...))
}

# @export
parse_yaml <-  function(...) {
  maybe_as_yaml(yaml::yaml.load(...))
}

# @export
as_yaml <- function(x)
  maybe_as_yaml(as.list(x))


# @export
yaml <- function(...)
  as_yaml(rlang::dots_list(..., .named = TRUE))

maybe_as_yaml <- function(x) {
  if (is.null(x))
    return(NULL)

  if(is.atomic(x) && length(x) != 1L)
    x <- as.list(x)
  if(is.list(x))
    class(x) <- "yaml"
  x
}

# @export
`$.yaml` <- function(x, ...)
  maybe_as_yaml(unclass(x)[[...]])
  # no partial matching, preserve 'yaml' class on sublists


# @export
`[[.yaml` <- function(x, ...)
  maybe_as_yaml(NextMethod())


# @export
`[.yaml` <- `[[.yaml`

# @exportS3Method
#' @importFrom utils str
str.yaml <- function(x, ...) {
  cat("YAML ")
  str(unclass(x), ...)
}

registerS3method("print", "yaml", print.yaml)
registerS3method("$", "yaml", `$.yaml`)
registerS3method("[[", "yaml", `[[.yaml`)
registerS3method("[", "yaml", `[.yaml`)
registerS3method("str", "yaml", str.yaml)
