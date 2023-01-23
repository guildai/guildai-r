

print.yaml <- function(x, file = "", ..., append = FALSE) {
  out <- encode_yaml(x, ...)
  for (f in file)
    cat(out, file = f, sep = "\n", append = append)

  invisible(out)
}


read_yaml <- function(file, ...) {
  yaml_load_args <- utils::modifyList(
    list(file = file,
         handlers = list(seq = identity)), # don't simplify lists
    list(...))
  maybe_as_yaml(do.call(yaml::read_yaml, yaml_load_args))
}

parse_yaml <-  function(string, ...) {
  yaml_load_args <- utils::modifyList(
    list(string = paste(string, collapse = "\n"),
         handlers = list(seq = identity), # don't simplify lists
         eval.expr = getOption("yaml.eval.expr", TRUE)), # eval !expr by default
    list(...))
  maybe_as_yaml(do.call(yaml::yaml.load, yaml_load_args))
}

as_yaml <- function(x)
  maybe_as_yaml(as.list(x))

#' @importFrom utils modifyList
encode_yaml <- function(x, ...) {
  out <- do.call(yaml::as.yaml, c(
    list(maybe_as_yaml(x)),
    utils::modifyList(list(
      precision = 15L,
      indent.mapping.sequence = TRUE,
      handlers = list(complex = as.character) # no complex type supported
    ),
    list(...))))
  out <- strsplit(out, "\n", fixed = TRUE)[[1L]]
  out
}

# TODO: merge guild.yml w/ op frontmatter (or ../guild.yml, ../../guild.yml)
# TODO: support source(chdir = TRUE) somehow?
# TODO: infer sourecode.root = ".." based on .Rproj or here::here()?

yaml <- function(...)
  as_yaml(rlang::dots_list(..., .named = TRUE))

maybe_as_yaml <- function(x) {
  if (is.null(x))
    return(NULL)

  if(is.atomic(x) && length(x) != 1L)
    x <- as.list(x)
  if(is.list(x) && is.null(oldClass(x)))
    class(x) <- "yaml"
  x
}


`$.yaml` <- function(x, ...)
  maybe_as_yaml(unclass(x)[[...]])
  # no partial matching, preserve 'yaml' class on sublists


`[[.yaml` <- function(x, ...)
  maybe_as_yaml(NextMethod())


`[.yaml` <- `[[.yaml`

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
