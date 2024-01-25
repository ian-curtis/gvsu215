#' Create a summary table for a linear regression test
#'
#' `infer_reg()` creates a small table summarizing a linear regression test. The traditional columns
#'    of `t` and `p-value` are removed by default but a full table can be accessed with simple = FALSE.
#'
#' @inheritParams infer_1prop
#' @param formula The variables to run the test on, in formula syntax. Passed on to [stats::lm()].
#' @param reduced Should a simple table be created (i.e., removal of the `t` and `p-value` columns)?
#'    Defaults to "yes".
#'
#' @return An object of class flextable. In an interactive session, results are viewable immediately.
#' @export
#'
#' @examples
#' infer_reg(mtcars, drat~wt)
#' infer_reg(mtcars, drat~wt, reduced = "no")
#' infer_reg(mtcars, drat~wt, digits = 4)
#' infer_reg(mtcars, drat~wt + qsec)
infer_reg <- function(data, formula, digits = 3, caption = NULL, reduced = c("yes", "no")) {

  # check for empty strings and make them actual NAs
  data <- tibble::as_tibble(data) %>%
    dplyr::mutate(dplyr::across(where(is.character), ~na_if(., "")))

  # error catching
  check_test(stats::lm(formula, data = data))

  reduced <- base::match.arg(reduced)

  # code
  var1 <- formula[[2]]
  var1_str <- base::deparse(base::substitute(var1))

  var2 <- formula[[3]]
  var2_str <- base::deparse(base::substitute(var2))


  # n_na <- find_na(data, formula, n = 2)

  # n1 <- base::nrow(data) - n_na[[1]]
  # n2 <- base::nrow(data) - n_na[[2]]

  model <-  stats::lm(formula, data = data)

  a_glance <- broom::glance(model)

  # build caption
  if (base::is.null(caption)) {

    caption <- base::paste("Linear Model Coefficients Table \n",
                           "Degrees of Freedom:", a_glance$df, "\n",
                           "R-Squared:", round(a_glance$r.squared, digits))

  } else {

    caption <- base::paste(caption, "\n",
                           "Degrees of Freedom:", a_glance$df, "\n",
                           "R-Squared:", round(a_glance$r.squared, digits))

  }

  # build table without p-value and test statistic
  if (reduced == "yes") {

    broom::tidy(model) %>%
      dplyr::select(-statistic, -p.value) %>%
      finalize_tbl(digits = digits, caption = caption) %>%
      flextable::set_header_labels(term = "Term", estimate = "Estimate", std.error = "Standard Error")

  } else if (reduced == "no") { # build table with p-value and test statistic

    broom::tidy(model) %>%
      dplyr::mutate(p.value = base::ifelse(p.value < 0.0001,
                                     "< 0.0001",
                                     base::format.pval(p.value, digits = digits))) %>%
      finalize_tbl(digits = digits,
                   caption = base::paste(caption,
                                         "\n Overall F: ",
                                         base::round(base::summary(model)$fstatistic, digits))) %>%
      flextable::set_header_labels(term = "Term", estimate = "Estimate", std.error = "Standard Error",
                                   statistic = "t", p.value = "p-value")

  }
}


#' Create a summary table for a chi-squared test
#'
#' `infer_chisq()` creates a tidy summary table for results of a chi-squared test. Alternatively, you can
#'    select to see expected counts (`type = "expected"`) or observed counts (`type = "observed"`).
#'
#' @inheritParams infer_1prop
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

  # check for empty strings and make them actual NAs
  data <- tibble::as_tibble(data) %>%
    dplyr::mutate(dplyr::across(where(is.character), ~na_if(., "")))

  # error catching
  type <- match.arg(type)

  check_test(stats::chisq.test(mosaic::tally(formula, data = data)))

  # code
  var1 <- formula[[2]]
  var1_str <- base::deparse(base::substitute(var1))
  var1_lvls <- data %>%
    tibble::as_tibble() %>%
    dplyr::mutate("{var1}" := base::factor({{ var1 }})) %>%
    dplyr::select({{ var1 }}) %>%
    base::unique() %>%
    base::nrow()

  var2 <- formula[[3]]
  var2_str <- base::deparse(base::substitute(var2))
  var2_lvls <- data %>%
    tibble::as_tibble() %>%
    dplyr::mutate("{var2}" := base::factor({{ var2 }})) %>%
    dplyr::select({{ var2 }}) %>%
    base::unique() %>%
    base::nrow()

  chisq_test <- stats::chisq.test(mosaic::tally(formula, data = data))

  if (type == "test") { # chi sq test results

    if (base::is.null(caption)) {

      caption <- base::paste("\uAB53\u00b2 Analysis of", var1_str, "and", var2_str)

    }

    broom::tidy(chisq_test) %>%
      dplyr::select(-method) %>%
      dplyr::mutate(p.value = base::ifelse(p.value < 0.0001,
                                     "< 0.0001",
                                     base::format.pval(.data$p.value, digits = digits))) %>%
      finalize_tbl(digits = digits, caption = caption, striped = FALSE) %>%
      flextable::set_header_labels(statistic = "\uAB53\u00b2", p.value = "p-value", parameter = "df")


  } else if (type == "expected") { # expected counts

    if (base::is.null(caption)) {

      caption <- base::paste("Expected Counts for", var1_str, "and", var2_str)

    }

    chisq_test$expected %>%
      tibble::as_tibble(rownames = var1_str) %>%
      finalize_tbl(digits = 1, caption = caption) %>%
      flextable::add_header_row(values = c("", var2_str),
                                colwidths = c(1, var2_lvls)) %>%
      flextable::hline(i = 1, part = "header") %>%
      flextable::hline(i = 1, j = 1, part = "header", border = officer::fp_border(color = NA)) %>%
      flextable::vline(j = 1, border = officer::fp_border(width = 2))

  } else if (type == "observed") { # observed counts

    if (base::is.null(caption)) {

      caption <- base::paste("Observed Counts for", var1_str, "and", var2_str)

    }

    chisq_test$observed %>%
      tibble::as_tibble() %>%
      tidyr::pivot_wider(names_from = {{ var2 }}, values_from = .data$n) %>%
      janitor::adorn_totals(c("row", "col")) %>%
      dplyr::mutate(Total = as.integer(.data$Total)) %>%
      finalize_tbl(digits = digits, caption = caption) %>%
      flextable::add_header_row(values = c("", var2_str, ""),
                                colwidths = c(1, var2_lvls, 1)) %>%
      flextable::vline(j = c(1, var2_lvls + 1), border = officer::fp_border(width = 1.2)) %>%
      flextable::hline(i = 1, part = "header") %>%
      flextable::hline(i = 1, j = c(1, var2_lvls + 2), part = "header",
                       border = officer::fp_border(color = NA)) %>%
      flextable::hline(i = var1_lvls, border = officer::fp_border(width = 1.2))

  }

}

#' Create a ANOVA table
#'
#' @inheritParams infer_2prop_int
#'
#' @return An object of class flextable. If in an interactive session, results are viewable immediately.
#' @export
#'
#' @examples
#' infer_anova(mtcars, drat~gear)
#' infer_anova(mtcars, drat~gear, digits = 4)
infer_anova <- function(data, formula, digits = 3, caption = NULL) {

  # check for empty strings and make them actual NAs
  data <- tibble::as_tibble(data) %>%
    dplyr::mutate(dplyr::across(where(is.character), ~na_if(., "")))

  # code
  var1 <- formula[[2]]
  var1_str <- base::deparse(base::substitute(var1))

  var2 <- formula[[3]]
  var2_str <- base::deparse(base::substitute(var2))

  check_test(stats::lm(formula, data = data))

  # build caption
  if (base::is.null(caption)) {

    caption <- base::paste("ANOVA Table for", var1_str, "vs.", var2_str)

  }

  data <- data %>%
    dplyr::mutate("{var2}" := base::factor({{ var2 }}))

  broom::tidy(stats::aov(formula, data)) %>%
    dplyr::mutate(term = c("Between", "Within"),
                  p.value = base::ifelse(!is.na(p.value),
                                         base::ifelse(p.value < 0.0001,
                                                "< 0.0001",
                                                base::format.pval(p.value, digits = 3)),
                                         p.value),
                  df = base::as.integer(df)) %>%
    janitor::adorn_totals("row", fill = NA, cols = dplyr::starts_with(c("term", "df", "sumsq"))) %>%
    finalize_tbl(digits = digits, caption = caption, na_str = "") %>%
    flextable::set_header_labels(term = "Source", sumsq = "SS", meansq = "MS", statistic = "F", p.value = "p-value")

}




