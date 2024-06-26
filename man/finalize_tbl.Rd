% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{finalize_tbl}
\alias{finalize_tbl}
\title{INTERNAL: Add default theme options to tables}
\usage{
finalize_tbl(table, digits, striped = TRUE, caption = NULL, na_str = "NA")
}
\arguments{
\item{table}{A dataframe (to be coerced to a flextable object)}

\item{digits}{The number of digits to round double columns to}

\item{striped}{Should the table be striped alternating colors (white/grey)? Defaults to TRUE.}

\item{caption}{The table caption. Defaults to NULL (or no caption).}

\item{na_str}{What string should appear when an NA appears? Defaults to "NA".}
}
\value{
An object of class flextable.
}
\description{
\code{finalize_tbl()} is used internally to complete the table creation process. This function creates
a flextable object and adds theme choices and size specifications.
}
\examples{
finalize_tbl(mtcars, digits = 3, caption = "The mtcars Dataset")
}
\keyword{internal}
