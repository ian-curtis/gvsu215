% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{fit_tbl}
\alias{fit_tbl}
\title{INTERNAL: Fit a table properly}
\usage{
fit_tbl(table)
}
\arguments{
\item{table}{A flextable object to fit.}
}
\value{
A object of class flextable, autofit if interactive or HTML output, 6.5 inches otherwise.
}
\description{
INTERNAL: Fit a table properly
}
\examples{
flextable::flextable(mtcars) |>
  fit_tbl()
}
\keyword{internal}
