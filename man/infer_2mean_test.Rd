% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/infer_mean.R
\name{infer_2mean_test}
\alias{infer_2mean_test}
\title{Create a summary table for a two-sample mean test}
\usage{
infer_2mean_test(
  data,
  formula,
  digits = 3,
  mu0 = 0,
  alternative = c("notequal", "greater", "less"),
  conf_lvl = 0.95,
  caption = NULL
)
}
\arguments{
\item{data}{A data frame (or tibble).}

\item{formula}{The variables to run the test on, in formula syntax, \code{var1 ~ var2}.}

\item{digits}{The number of digits to round table values to. Defaults to 3.}

\item{mu0}{The null hypothesis value. Defaults to 0.}

\item{alternative}{The alternative hypothesis. Defaults to "notequal" (two sided p-value).
Other options include "greater" or "less". Use depends on your test.}

\item{conf_lvl}{The confidence level of the interval, entered as a value between 0 and 1.
Defaults to 0.95.}

\item{caption}{An override to the table caption. A sensible default is provided.}
}
\value{
An object of class flextable. In an interactive environment, results are viewable immediately.
}
\description{
Create a summary table for a two-sample mean test
}
\examples{
infer_2mean_test(mtcars, wt~vs)
infer_2mean_test(mtcars, wt~vs, alternative = "greater")
infer_2mean_test(mtcars, wt~vs, conf_lvl = .9)
}
