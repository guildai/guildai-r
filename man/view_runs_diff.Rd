% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/view.R
\name{view_runs_diff}
\alias{view_runs_diff}
\title{compare runs}
\usage{
view_runs_diff(
  runs = "1:2",
  output_dir = file.path(tempdir(), paste(ids, collapse = "-")),
  template = system.file("templates/compare-runs.qmd", package = "guildai"),
  viewer = getOption("guildai.viewer"),
  ...,
  include_dotguild = FALSE
)
}
\arguments{
\item{runs}{A runs selection for two runs. Defaults to the latest two runs.}

\item{output_dir}{Where to place the rendered html}

\item{template}{Report template}

\item{viewer}{Viewer to display training run information within (default
to an internal page viewer if available, otherwise to the R session
default web browser, \code{utils::browseURL()}).}

\item{...}{passed on to \code{quarto::quarto_render()}}

\item{include_dotguild}{whether the \code{".guild"} directory should be included
in the diff view.}
}
\value{
path to the generated html, invisibly
}
\description{
compare runs
}
\keyword{internal}
