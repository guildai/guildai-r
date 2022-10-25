


#' Render a Run Summary Report
#'
#' @param id run id. Defaults to the latest run.
#' @param output_dir directory where to place the rendered document.
#' @param template path to a parameterized quarto document.
#' @param viewer Viewer to display training run information within (default
#'   to an internal page viewer if available, otherwise to the R session
#'   default web browser, `utils::browseURL()`).


#' @param ... passed on to `quarto::quarto_render()`
#'
#' @return path to the generated html, invisibly
#' @export
view_run_report <- function(
  id = runs_info("1")$id,
  output_dir = file.path(tempdir(), id),
  template = system.file("templates/view-run.qmd", package = "guildai"),
  viewer = getOption("guildai.viewer"),
  ...)
{
  if(!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE)

  withr::local_dir(output_dir)

  if(!file.exists(basename(template)))
    file.copy(template, ".")

  if(!requireNamespace("quarto", quietly = TRUE))
    stop("Please install the {quarto} R package.")

  quarto::quarto_render(
    input = basename(template),
    execute_params = list(run_id = id),
    ...)

  html <- sprintf("%s.html", tools::file_path_sans_ext(basename(template)))
  view_page(normalizePath(html), getOption("guildai.viewer"))
  invisible(html)
}



view_page <- function(viewer_html, viewer) {

  if (is.null(viewer))
    viewer <- getOption("page_viewer", default = utils::browseURL)
  if (identical(viewer, getOption("page_viewer"))) {
    args <- list(url = viewer_html)
    if (!is.null(formals(viewer)$self_contained))
      args$self_contained <- TRUE
    do.call(viewer, args)
  } else {
    browser_viewer(dirname(viewer_html), viewer)(viewer_html)
  }

  invisible(viewer_html)

}


# non-rstudio viewer function
browser_viewer <- function(viewer_dir, browser = utils::browseURL) {

  function(url) {
    # determine help server port
    port <- tools::startDynamicHelp(NA)
    if (port <= 0)
      port <- tools::startDynamicHelp(TRUE)
    if (port <= 0) {
      warning("Unable to view run metrics (couldn't access help server port)",
              call. = FALSE)
      return(invisible(NULL))
    }

    # determine path to history html
    path <- paste("/session", basename(viewer_dir), basename(url), sep = "/")

    # build URL and browse it
    url <- paste0("http://127.0.0.1:", port, path)
    browser(url)
  }
}





#' compare runs
#'
#' @param ids, a length 2 character vector or run ids
#' @param output_dir where to place the rendered html
#' @param template report template
#' @param viewer Viewer to display training run information within (default
#'   to an internal page viewer if available, otherwise to the R session
#'   default web browser, `utils::browseURL()`).
#' @param ... passed on to `quarto::quarto_render()`
#'
#' @return path to the generated html, invisibly
#' @export
view_runs_diff <- function(
    ids = runs_info("1:2")$id,
    output_dir = file.path(tempdir(), paste(ids, collapse = "-")),
    template = system.file("templates/compare-runs.qmd", package = "guildai"),
    viewer = getOption("guildai.viewer"),
    ...)
{
  stopifnot(length(ids) == 2)

  if(!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE)

  withr::local_dir(output_dir)

  if(!file.exists(basename(template)))
    file.copy(template, ".")

  if(!requireNamespace("quarto", quietly = TRUE))
    stop("Please install the {quarto} R package.")

  quarto::quarto_render(
    input = basename(template),
    execute_params = list(run_id_1 = ids[1], run_id_2 = ids[2]),
    ...)

  html <- sprintf("%s.html", tools::file_path_sans_ext(basename(template)))
  view_page(normalizePath(html), getOption("guildai.viewer"))
  invisible(html)
}


if(FALSE) {
  Sys.setenv("GUILD_HOME" = "~/guild/fashion-mnist/.guild")
  view_run_report("61f6b7c3bc9d47dab2e6ef6632dc8797")
  compare_runs("61f6b7c3bc9d47dab2e6ef6632dc8797",
               "a9da9f78c5374b7289500458dab0d3e7")
}
