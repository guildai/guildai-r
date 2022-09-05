


# ' @importFrom yaml as.yaml yaml.load
print.yaml <- function(x, file = "") {
  out <- yaml::as.yaml(x,
                 precision = 16L,
                 indent.mapping.sequence = FALSE)
  for(f in file)
    cat(out, file = f)
  invisible(out)
}


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

as_yaml <- function(x) {
  class(x) <- "yaml"
  x
}

yaml <- function(...) {
  as_yaml(list(...))
}

registerS3method("print", "yaml", print.yaml)

#                  function(x) {
#   cat(yaml::as.yaml(x))
#   invisible(x)
# })

registerS3method("$", "yaml", function(x, name, ...) {
  # no partial matching, preserve 'yaml' class
  out <- unclass(x)[[name, ...]]
  if (is.null(out)) NULL
  else structure(out, class = "yaml")
})

registerS3method("[[", "yaml", function(x, ...) {
  out <- NextMethod()
  if(!is.null(out))
    class(out) <- "yaml"
  out
})
