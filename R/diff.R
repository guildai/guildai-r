


#' View a runs diff
#'
#' Renders and displays an HTML document showing the diffs between two run
#' directories.
#'
#' @param runs A runs selection for two runs. Defaults to the latest two runs.
#' @param ...,output_empty,exclude_directory_metadata Passed on to `diffoscope`.
#'   Pass `"--help"` for a printout of all arguments accepted.
#' @param output_html Where to place the rendered html
#' @param exclude Regex pattern for files or directories to exclude from the
#'   rendered report. By default, the guild internal metadata store `".guild"`
#'   is omitted.
#' @param viewer Viewer to display the rendered html. Defaults to
#'   `utils::browseURL()`, which opens a browser).
#'
#' @keywords internal
#'
#' @return path to the generated html, invisibly
view_runs_diff <-
  function(runs = 1:2,
           ...,
           output_html = tempfile("guild-diff-", fileext = ".html"),
           exclude = ".guild",
           output_empty = TRUE,
           exclude_directory_metadata = c("yes", "no", "auto", "recursive"),
           viewer = utils::browseURL) {

  run_dirs <- runs_info(runs)$dir
  if(length(run_dirs) > 2)
    run_dirs <- run_dirs[1:2]
  stopifnot(length(run_dirs) == 2)

  withr::local_dir(fs::path_common(run_dirs))
  run_dirs <- fs::path_rel(run_dirs)

  diffoscope_render(
    html = output_html,
    exclude = exclude,
    exclude_directory_metadata = match.arg(exclude_directory_metadata),
    output_empty = output_empty,
    ...,
    run_dirs
  )

  if(is.function(viewer))
    viewer(output_html)

  invisible(output_html)
}


diffoscope_render <- function(...) {
  if(!nzchar(Sys.which("diffoscope")))
    stop("Please install diffoscope and make sure it is on your PATH: https://diffoscope.org")

  system2t("diffoscope", as_cli_args(...))
}
