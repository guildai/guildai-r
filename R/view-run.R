


#' Render a Run Summary Report
#'
#' @param id run id
#' @param output_dir directory where to place the rendered document
#' @param template path to a paramaterized quarto document
#' @param ... passed on to `quarto::quarto_render()`
#'
#' @return path to the generated html, invisibly
#' @export
run_report <- function(
  id,
  output_dir = file.path(tempdir(), id),
  template = system.file("templates/view-run.qmd", package = "guildai"),
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





if(FALSE) {
  Sys.setenv("GUILD_HOME" = "~/guild/fashion-mnist/.guild")
  run_report("61f6b7c3bc9d47dab2e6ef6632dc8797")
}
