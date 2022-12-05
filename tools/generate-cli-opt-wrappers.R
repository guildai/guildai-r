#!/usr/bin/env Rscript

library(purrr)
library(tibble)
library(dplyr)
library(stringr)
library(commafree)
library(glue)
library(envir)

attach_eval({
  import_from(guildai, guild, str_drop_prefix, parse_yaml)
  import_from(magrittr, `%<>%`, not)
  import_from(tidyr, unnest)
  filter_out <- function(.data, ...) {
    dots <- lapply(quos(...), function(q) quo( not(!!q) ))
    filter(.data, !!!dots)
  }
})

parse_opt_term_field <- function(term) {
  # term can have a variable number of fields
  # '-h, --host HOST'     short_name long_name val
  # '-n, --no-open'       switch, short + long name
  # '--logging'           switch, only long name
  # '--start, --restart RUN'  multiple long_name + val

  term %>%
    strsplit(",?\\s+") %>%
    map(function(x) {

      if (startsWith(x[1], "-") && !startsWith(x[1], "--")) {
        short_name <- x[1]
        x <- x[-1]
      } else
        short_name <- NA_character_

      name <- character()
      while (length(x) && startsWith(x[1], "--")) {
        name <- c(name, x[1])
        x <- x[-1]
      }

      if (length(x))
        default <- NULL
      else
        # it's a bool switch
        default <- FALSE

      tibble(short_name, name, default = list(default))
    })
}


# opt_name_to_rarg_name <- function(x) {
opt_r_name_to_cli_name <- function(x) {
  x <- sub("^\\.\\.", "--", x)
  x <- sub("^\\.", "-", x)
  needs_prefix <- nzchar(x) & !startsWith(x, "-")
  x[needs_prefix] <- paste0("--", x[needs_prefix])
  x <- gsub("_", "-", x, fixed = TRUE)
  x
}

opt_cli_name_to_r_name <- function(x) {
  x <- yasp::unwrap(x, "`", n_pairs = 1)
  x <- str_drop_prefix(x, "--")
  x <- gsub("-", "_", x, fixed = TRUE)
  x
}


tidy_opts <- function(command, omit = NULL) {
  x <- guild(command, "--help", env = "GUILD_HELP_JSON=1", stdout = TRUE) |>
    paste0(collapse = "") |>
    parse_yaml()

  opts <- transpose(x$options) %>% map(flatten_chr) %>% as_tibble()
  opts <- opts %>%
    mutate(info = parse_opt_term_field(term)) %>%
    unnest(info) %>%
    mutate(
      cli_name = name,
      name = name %>%
        str_drop_prefix("--") %>%
        str_replace_all(fixed("-"), "_")
    )

  # replace cli names w/ name:  --proto  -->  `proto`
  # prefix switches w/ "(bool)"
  opts$help <- opts$help %>%
    imap_chr(function(h, i) {
      for(r in seq_len(nrow(opts)))
        h <- str_replace_all(h,
                             fixed(opts$cli_name[[r]]),
                             backtick(opts$name[[r]]))
      if(is.logical(opts$default[[i]]))
        h <- str_c("(bool) ", h)
      h
    })

  help <- x$help
  help <- gsub(".\b", "", help)
  for(r in seq_len(nrow(opts)))
    help <- str_replace_all(help,
                            fixed(opts$cli_name[[r]]),
                            opts$name[[r]])

  opts <- opts %>% filter(!name %in% c(omit, "help", "yes"))

  attr(opts, "command") <- x$usage$prog
  attr(opts, "help") <- help
  opts
}


gen_wrapper_text <- function(command, passes_on_to = NULL, omit = NULL) {
  opts <- tidy_opts(command, omit = omit)

  fn_name <- attr(opts, "command") %>%
    gsub(" ", "_", .) %>% paste0(., "_cli")

  roxy_params <-
    c("@param ... passed on to the `guild` executable. Arguments are automatically quoted with `shQuote()`, unless they are protected with `I()`. Pass `'--help'` or `help = TRUE` to see all options.",
      glue::glue_data(opts, "@param {name} {help}"))

  roxy_desc <- attr(opts, "help") %>%
    strsplit("\n", fixed = TRUE) %>% .[[1L]] %>%
    c("", "")

  roxy <- paste("#'", c %(% {
    # attr(opts, "command")
    fn_name
    roxy_desc
    roxy_params
  })

  frmls <- c(alist(... = ), deframe(select(opts, name, default)))
  body <- if(is.null(passes_on_to))
    quote(as_guild_args(as.list.environment(environment()), ...))
  else
    bquote(c(as_guild_args(as.list.environment(environment())),
           .(substitute(passes_on_to))(...)))

  body <- substitute({body}, list(body = body))

  wrapper_fn <- as.function.default(c(frmls, body))

  # all.names=FALSE: kludge to omit the `...` symbol.

  txt <- c(roxy,
           # paste(backtick(fn_name), "<-"), deparse(wrapper_fn),
           # paste(backtick(attr(opts, "command")), "<-"), deparse(wrapper_fn),
           paste(fn_name, "<-"), deparse(wrapper_fn),
           "\n\n")
  txt <- trimws(txt)
  glue::as_glue(txt) # noquote print method
}; #gen_wrapper_text("run") %>% invisible() #cat()#%>% .$term %>% parse_opt_term_field()




writeLines(c %(% {

  gen_wrapper_text("run", omit = c(
    "break", "break_on_error", "random_seed", "debug_sourcecode"))
  gen_wrapper_text("api runs")
  gen_wrapper_text("view")
  gen_wrapper_text("merge")
  gen_wrapper_text("select")

}, "R/auto-generated-cli-doc-stubs.R")

# TODO: guild_select_opts should be it's own func that other opts inherit dot params to
# TODO: filter out irrelevant help sections from roxy descriptions, like "Breakpoints" for `guild run`


# df <- tidy_opts("run")
#
# select_opts <- tidy_opts("select")
#
# gen_wrapper_text("select")

devtools::document()
