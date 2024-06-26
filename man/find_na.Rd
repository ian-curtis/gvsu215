% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{find_na}
\alias{find_na}
\title{INTERNAL: Find missing values}
\usage{
find_na(data, formula, n = 1)
}
\arguments{
\item{data}{A data frame}

\item{formula}{One or two variables in formula notation: \code{~var1} or \code{var1 ~ var2}.}

\item{n}{How many variables to analyze. Acceptable values are 1 or 2.}
}
\value{
An number (n = 1) or a list of two numbers (n = 2)
}
\description{
Used internally to calculate the number of missing values in one or two variables.
}
\examples{
find_na(mtcars, ~cyl)
find_na(mtcars, wt~cyl, n = 2)
}
\keyword{internal}
