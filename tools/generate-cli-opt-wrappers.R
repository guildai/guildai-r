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
  filter_out <- function(.data, ...) {
    dots <- lapply(quos(...), function(q) quo( not(!!q) ))
    filter(.data, !!!dots)
  }
})

parse_opt_term_field <- function(term) {
  # term can have up to three fields
  # '-h, --host HOST'     short_name long_name val
  # '-n, --no-open'       switch, short + long name
  # '--logging'           switch, only long name

  term %>%
    strsplit(",?\\s+") %>%
    map_dfr(function(x) {

      if (startsWith(x[1], "-") && !startsWith(x[1], "--")) {
        short_name <- x[1]
        x <- x[-1]
      } else
        short_name <- NA_character_

      if (startsWith(x[1], "--")) {
        name <- x[1]
        x <- x[-1]
      }

      if (length(x))
        default <- NULL
      else
        # it's a bool switch
        default <- FALSE

      list(short_name = short_name, name = name, default = list(default))
    }) %>%
    mutate(name = name %>%
             str_drop_prefix("--") %>%
             str_replace_all(fixed("-"), "_"))

}


replace_cli_arg_names_with_r_arg_names <- function(x)  {
  m <- gregexec("`--[[:alnum:]-]+`", x)
  regmatches(x, m) <- regmatches(x, m) %>%
    map(~sprintf("\\code{%s}", opt_cli_name_to_r_name(.x)))
  x
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
  x <- guildai:::str_drop_prefix(x, "--")
  x <- gsub("-", "_", x, fixed = TRUE)
  x
}


gen_wrapper_text <- function(command, passes_on_to = NULL, omit = NULL) {
  x <- guild(command, "--help", env = "GUILD_HELP_JSON=1", stdout = TRUE) |>
    paste0(collapse = "") |>
    parse_yaml()

  fn_name <- x$usage$prog %>% gsub(" ", "_", .) %>% paste0(., "_opts")

  opts <- transpose(x$options) %>% map(flatten_chr) %>% as_tibble()
  opts <- bind_cols(opts, parse_opt_term_field(opts$term))
  opts <- opts %>% filter(!name %in% c(omit, "help", "yes"))
  opts$help %<>% replace_cli_arg_names_with_r_arg_names()

  roxy_params <-
    c("@param ... passed on to the `guild` executable. Pass `'--help'` to see all options.",
      glue::glue_data(opts, "@param {name} {help}"))

  roxy_desc <- x$help %>%
    strsplit("\n", fixed = TRUE) %>% .[[1L]] %>%
    replace_cli_arg_names_with_r_arg_names() %>%
    gsub(".\b", "", .) %>% c("", "")

  roxy <- paste("#'", c %(% {
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

  wrapper_fn <- as.function.default(c(frmls, body))

  # all.names=FALSE: kludge to omit the `...` symbol.

  txt <- c(roxy,
           paste(fn_name, "<-"), deparse(wrapper_fn),
           "\n\n")
  trimws(txt)
}; #gen_wrapper_text("run") %>% invisible() #cat()#%>% .$term %>% parse_opt_term_field()




writeLines(c %(% {

  gen_wrapper_text("view")
  gen_wrapper_text("run", omit = c(
    "break", "break_on_error", "random_seed", "debug_sourcecode"))
  gen_wrapper_text("select")
  gen_wrapper_text("merge")

}, "R/auto-generated-cli-opt-wrappers.R")

devtools::document()
# TODO: guild_select_opts should be it's own func that other opts inherit dot params to
# TODO: filter out irrelevant help sections from roxy descriptions, like "Breakpoints" for `guild run`
