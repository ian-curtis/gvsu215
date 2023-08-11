#' Create a summary table for a linear regression test
#'
#' `infer_reg()` creates a small table summarizing a linear regression test. The traditional columns
#'    of `t` and `p-value` are removed by default but a full table can be accessed with simple = FALSE.
#'
#' @inheritParams infer_prop1
#' @param formula The variables to run the test on, in formula syntax. Passed on to [stats::lm()].
#' @param reduced Should a simple table be created (i.e., removal of the `t` and `p-value` columns)?
#'    Defaults to TRUE.
#'
#' @return An object of class flextable. In an interactive session, results are viewable immediately.
#' @export
#'
#' @examples
#' infer_reg(mtcars, drat~wt)
#' infer_reg(mtcars, drat~wt, reduced = FALSE)
#' infer_reg(mtcars, drat~wt, digits = 4)
#' infer_reg(mtcars, drat~wt + qsec)
infer_reg <- function(data, formula, digits = 3, caption = NULL, reduced = TRUE) {

  # error catching
  check_test(stats::lm(formula, data = data))


  # code
  var1 <- formula[[2]]
  str_of_var1 <- base::deparse(base::substitute(var1))

  var2 <- formula[[3]]
  str_of_var2 <- base::deparse(base::substitute(var2))


  # n_na <- find_na(data, formula, n = 2)

  # n1 <- base::nrow(data) - n_na[[1]]
  # n2 <- base::nrow(data) - n_na[[2]]

  model <-  stats::lm(formula, data = data)

  a_glance <- broom::glance(model)

  if (base::is.null(caption)) {

    caption <- base::paste("Linear Model Coefficients Table \n",
                           "Degrees of Freedom:", a_glance$df, "\n",
                           "R-Squared:", round(a_glance$r.squared, digits))

  } else {

    caption <- base::paste(caption, "\n",
                           "Degrees of Freedom:", a_glance$df, "\n",
                           "R-Squared:", round(a_glance$r.squared, digits))

  }

  if (reduced == TRUE) {

    broom::tidy(model) %>%
      dplyr::select(-statistic, -p.value) %>%
      finalize_tbl(digits = digits, caption = caption) %>%
      flextable::set_header_labels(term = "Term", estimate = "Estimate", std.error = "Standard Error")

  } else if (reduced == FALSE) {

    broom::tidy(model) %>%
      dplyr::mutate(p.value = base::format.pval(p.value, digits = digits)) %>%
      finalize_tbl(digits = digits,
                   caption = base::paste(caption,
                                         "\n Overall F: ",
                                         base::round(base::summary(model)$fstatistic, digits))) %>%
      flextable::set_header_labels(term = "Term", estimate = "Estimate", std.error = "Standard Error",
                                   statistic = "t", p.value = "p-value")

  }

  # kbl(digits = 3,
  #     caption = paste0("Linear Model Coefficients Table: <br> F-test of ",
  #                      round(a_glance$statistic, 3), " on ", a_glance$df, " and ",
  #                      a_glance$df.residual, " degrees of freedom with an overall p-value of ",
  #                      overall_p, ".<br>", "R-Squared: ", round(a_glance$r.squared, 3), ".")) %>%
  # kable_styling(c('condensed', 'bordered', 'striped'), full_width = F) %>%
  # row_spec(0, extra_css = "border-bottom: 3px solid black")

}


#' Create a summary table for a chi-squared test
#'
#' `infer_chisq()` creates a tidy summary table for results of a chi-squared test. Alternatively, you can
#'    select to see expected counts (`type = "expected"`) or observed counts (`type = "observed"`).
#'
#' @inheritParams infer_prop1
#' @param type The kind of output to receive. Valid options are either "test" (the default), "expected",
#'    or "observed".
#'
#' @return An object of class flextable.
#' @export
#'
#' @examples
#' suppressWarnings(infer_chisq(mtcars, cyl~gear))
#' suppressWarnings(infer_chisq(mtcars, cyl~gear, type = "expected"))
#' suppressWarnings(infer_chisq(mtcars, cyl~gear, type = "observed"))
infer_chisq <- function(data, formula, type = c("test", "expected", "observed"), digits = 3, caption = NULL) {

  # error catching
  type <- match.arg(type)

  check_test(stats::chisq.test(mosaic::tally(formula, data = data)))

  # code
  var1 <- formula[[2]]
  str_of_var1 <- base::deparse(base::substitute(var1))

  var2 <- formula[[3]]
  str_of_var2 <- base::deparse(base::substitute(var2))

  chisq_test <- stats::chisq.test(mosaic::tally(formula, data = data))

  if (type == "test") {

    if (base::is.null(caption)) {

      caption <- base::paste("Chi-Squared Analysis of", str_of_var1, "and", str_of_var2)

    }

    broom::tidy(chisq_test) %>%
      dplyr::select(-method) %>%
      dplyr::mutate(p.value = base::format.pval(p.value, digits = digits)) %>%
      finalize_tbl(digits = digits, caption = caption) %>%
      flextable::set_header_labels(statistic = "X-squared", p.value = "p-value", parameter = "df")
    # kbl(digits = 3,
    #     caption = paste("Chi-Squared Test of", "X", "and", "X"),
    #     col.names = c('Statistic', 'p-value', 'Degrees of Freedom')) %>%
    # kable_styling(c('condensed', 'bordered', 'striped'), full_width = F) %>%
    # row_spec(0, extra_css = "border-bottom: 3px solid black")

  } else if (type == "expected") {

    if (base::is.null(caption)) {

      caption <- base::paste("Expected Counts for", str_of_var1, "and", str_of_var2)

    }

    chisq_test$expected %>%
      tibble::as_tibble(rownames = str_of_var1) %>%
      finalize_tbl(digits = 1, caption = caption) %>%
      flextable::vline(j = 1)
    # kbl(digits = 3, caption = "Expected Counts for Var and Var") %>%
    # kable_styling(c('condensed', 'bordered', 'striped'), full_width = F) %>%
    # row_spec(0, extra_css = "border-bottom: 3px solid black")

  } else if (type == "observed") {

    if (base::is.null(caption)) {

      caption <- base::paste("Observed Counts for", str_of_var1, "and", str_of_var2)

    }

    chisq_test$observed %>%
      tibble::as_tibble() %>%
      tidyr::pivot_wider(names_from = {{ var2 }}, values_from = n) %>%
      janitor::adorn_totals(c("row", "col")) %>%
      dplyr::mutate(Total = as.integer(Total)) %>%
      finalize_tbl(digits = digits, caption = caption)
    # kbl(caption = "Observed Counts for Var and Var") %>%
    # kable_styling(c('condensed', 'bordered', 'striped'), full_width = F) %>%
    # row_spec(nrow(chisq_test$observed) + 1, bold = TRUE) %>%
    # column_spec(ncol(chisq_test$expected) + 2, bold = TRUE) %>%
    # row_spec(0, extra_css = "border-bottom: 3px solid black")

  }

}

#' Create a summary table
#'
#' @inheritParams infer_prop2_int
#'
#' @return An object of class flextable. If in an interactive session, results are viewable immediately.
#' @export
#'
#' @examples
#' infer_anova(mtcars, cyl~gear)
#' infer_anova(mtcars, cyl~gear, digits = 4)
infer_anova <- function(data, formula, digits = 3, caption = NULL) {

  # code
  var1 <- formula[[2]]
  str_of_var1 <- base::deparse(base::substitute(var1))

  var2 <- formula[[3]]
  str_of_var2 <- base::deparse(base::substitute(var2))

  check_test(stats::lm(formula, data = data))

  if (base::is.null(caption)) {

    caption <- base::paste("ANOVA Table for", str_of_var1, "vs.", str_of_var2)

  }

  model <-  stats::lm(formula, data = data)

  broom::tidy(stats::anova(model)) %>%
    dplyr::mutate(term = c("Between", "Within"),
                  p.value = base::ifelse(!is.na(p.value),
                                         base::format.pval(p.value, digits = 3),
                                         p.value)) %>%
    janitor::adorn_totals("row", fill = NA, cols = dplyr::starts_with(c("term", "df", "sumsq"))) %>%
    finalize_tbl(digits = digits, caption = caption, na_str = "") %>%
    flextable::set_header_labels(term = "Source", sumsq = "SS", meansq = "MS", statistic = "F", p.value = "p-value")

}




