% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diff.R
\name{view_runs_diff2}
\alias{view_runs_diff2}
\title{View a runs diff}
\usage{
view_runs_diff2(
  runs = 1:2,
  ...,
  output_html = tempfile("guild-diff-", fileext = ".html"),
  exclude = ".guild",
  output_empty = TRUE,
  exclude_directory_metadata = c("yes", "no", "auto", "recursive"),
  viewer = utils::browseURL
)
}
\arguments{
\item{runs}{A runs selection for two runs. Defaults to the latest two runs.}

\item{..., output_empty, exclude_directory_metadata}{Passed on to \code{diffoscope}.
Pass \code{"--help"} for a printout of all arguments accepted.}

\item{output_html}{Where to place the rendered html}

\item{exclude}{Regex pattern for files or directories to exclude from the
rendered report. By default, the guild internal metadata store \code{".guild"}
is omitted.}

\item{viewer}{Viewer to display the rendered html. Defaults to
\code{utils::browseURL()}, which opens a browser).}
}
\value{
path to the generated html, invisibly
}
\description{
Renders and displays an HTML document showing the diffs between two run
directories.
}
\keyword{internal}
