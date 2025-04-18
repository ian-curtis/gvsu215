% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{plot_box}
\alias{plot_box}
\title{Create a simple boxplot}
\usage{
plot_box(
  data,
  formula,
  fill = "grey80",
  breaks = NULL,
  orient = c("vertical", "horizontal"),
  title = NULL,
  subtitle = c("show", "hide"),
  ...
)
}
\arguments{
\item{data}{A data frame.}

\item{formula}{Variables entered in formula notation. Either \code{~var} for a one-variable boxplot
or \code{var1~var2} for a grouped boxplot where \code{var2} is a grouping variable.}

\item{fill}{The fill color for the boxplot. Entered as a character.}

\item{breaks}{A vector of length 3 (start, stop, step) specifying how the x-scale should be broken up.}

\item{orient}{The orientation for the plot (either "vertical", the default, or "horizontal"). As a
shortcut, "v" and "h" may be used.}

\item{title}{An override for the title of the plot. A sensible default is provided.}

\item{subtitle}{A switch for hiding the default subtitle. One of "show" or "hide".}

\item{...}{Extra title arguments passed on to \code{\link[ggformula:gf_aux]{ggformula::gf_labs()}} (which feeds to \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}}).}
}
\value{
A ggplot object. In an interactive environment, results are viewable immediately.
}
\description{
\code{plot_box()} builds a simple, pre-themed boxplot on one variable alone or grouped by another variable.
}
\examples{
plot_box(mtcars, ~wt)
plot_box(mtcars, wt~gear, fill = 'orangered4')
plot_box(mtcars, ~wt, breaks = seq(1, 6, 0.5))
plot_box(mtcars, wt~gear)
plot_box(mtcars, wt~gear, breaks = seq(1, 6, 0.5))
}
