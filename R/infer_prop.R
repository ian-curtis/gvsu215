#' Create a summary table for a one-sample proportion interval
#'
#' @param data A data frame (or tibble).
#' @param formula The variable to run the test on, in formula syntax, `~var`.
#' @param success The data value that constitutes a "success".
#' @param digits The number of digits to round table values to. Defaults to 3.
#' @param conf_lvl The confidence level of the interval, entered as a value between 0 and 1.
#'    Defaults to 0.95.
#' @param caption An override to the table caption. A sensible default is provided.
#'
#' @return An object of class flextable. In an interactive environment, results are viewable immediately.
#' @export
#'
#' @examples
#' infer_1prop_int(mtcars, ~vs, success = 1)
#' infer_1prop_int(mtcars, ~vs, success = 1, conf_lvl = 0.90)
infer_1prop_int <- function(data, formula, success = NULL, digits = 3,
                            conf_lvl = 0.95, caption = NULL) {

  # check for empty strings and make them actual NAs
  data <- tibble::as_tibble(data) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., "")))

  # error catching
  check_conf_lvl(conf_lvl)

  if (base::is.null(success)) {

    cli::cli_abort("A value for what a success is in this situation must be provided.")

  }

  check_test(mosaic::prop.test(formula, data = data, conf.level = conf_lvl, success = success,
                               correct = FALSE))

  #code

  var <- formula[[2]]
  var_str <- base::deparse(base::substitute(var))

  prop_test <- mosaic::prop.test(formula, data = data, conf.level = conf_lvl, success = success,
                                 correct = FALSE)
  cl <-  base::paste0(conf_lvl*100, "%")

  # build caption
  if (base::is.null(caption)) {

    caption <- base::paste("One-Sample Proportion Confidence Interval on Variable", var_str,
                           "\nSuccesses:", success,
                           "\nConfidence Level:", cl)

  } else {

    caption <- base::paste(caption, "\nSuccesses:", success,
                           "\nConfidence Level:", cl)

  }


  n_success <- data %>%
    dplyr::filter({{ var }} == success) %>%
    base::nrow()

  n_na <- find_na(data, formula)

  phat <- prop_test$estimate[[1]]
  n <- base::nrow(data) - n_na
  se <- base::sqrt((phat * (1 - phat)) / n)
  z_star <- round(stats::qnorm((1 - conf_lvl) / 2, lower.tail = FALSE), 3)
  moe <- se*z_star


  broom::tidy(prop_test) %>%
    dplyr::mutate(n_success = n_success,
                  na = n_na,
                  n = n,
                  se = se,
                  conf.low = estimate - moe,
                  conf.high = estimate + moe
    ) %>%
    dplyr::select(n_success, na, n, estimate, se, conf.low, conf.high) %>%
    finalize_tbl(digits = digits,
                 caption = caption,
                 striped = FALSE) %>%
    flextable::set_header_labels(n_success = "n\nSuccesses", na = "n\nMissing",
                                 estimate = "p\u0302", se = "Standard\nError",
                                 conf.low = base::paste(cl, "\nInterval\nLower"),
                                 conf.high = base::paste(cl, "\nInterval\nUpper")) %>%
    fit_tbl()

}

#' Create a summary table for a one-sample proportion test
#'
#' @inheritParams infer_1prop_int
#' @param p0 The null hypothesis value. Defaults to 0.5.
#' @param alternative The alternative hypothesis. Defaults to "notequal" (two sided p-value).
#'    Other options include "greater" or "less". Use depends on your test.
#' @return An object of class flextable. In an interactive environment, results are viewable immediately.
#' @export
#'
#' @examples
#' infer_1prop_test(mtcars, ~vs, success = 1)
#' infer_1prop_test(mtcars, ~vs, success = 1, alternative = "less")
#' infer_1prop_test(mtcars, ~vs, success = 1, conf_lvl = 0.90)
#' infer_1prop_test(mtcars, ~vs, success = 1, p0 = 0.4)
infer_1prop_test <- function(data, formula, success = NULL, p0 = 0.5, digits = 3,
                             alternative = c("notequal", "greater", "less"),
                             conf_lvl = 0.95, caption = NULL) {

  # check for empty strings and make them actual NAs
  data <- tibble::as_tibble(data) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., "")))

  # error catching
  check_conf_lvl(conf_lvl)

  alternative = base::match.arg(alternative)

  if (alternative == "notequal") {
    alt_hyp <- "two.sided"
    p_header <- "Two Sided"
  } else if (alternative == "greater") {
    alt_hyp <- alternative
    p_header <- "One Sided (Greater Than)"
  } else {
    alt_hyp <- alternative
    p_header <- "One Sided (Less Than)"
  }

  if (base::is.null(success)) {

    cli::cli_abort("A value for what a success is in this situation must be provided.")

  }

  check_test(mosaic::prop.test(formula, data = data, p = p0, conf.level = conf_lvl,
                               alternative = alt_hyp, success = success,
                               correct = FALSE))

  #code

  var <- formula[[2]]
  var_str <- base::deparse(base::substitute(var))

  # build caption
  if (base::is.null(caption)) {

    caption <- base::paste("One-Sample Proportion Test on Variable", var_str,
                           "\nSuccesses:", success,
                           "\nNull Value:", p0,
                           "\np-value Reported:", p_header)

  } else {

    caption <- base::paste(caption, "\nSuccess:", success,
                           "\nNull Value:", p0,
                           "\np-value Reported:", p_header)

  }

  prop_test <- mosaic::prop.test(formula, data = data, p = p0, conf.level = conf_lvl,
                                 alternative = alt_hyp, success = success,
                                 correct = FALSE)

  cl <-  base::paste0(conf_lvl*100, "%")

  n_success <- data %>%
    dplyr::filter({{ var }} == success) %>%
    base::nrow()

  n_na <- find_na(data, formula)

  n <- base::nrow(data) - n_na

  ### Manual Calculations for Wald's Method not Wilson's ###

  z_num <- prop_test$estimate - p0
  se <- base::sqrt((p0*(1 - p0)) / n)
  z <- z_num / se

  broom::tidy(prop_test) %>%
    dplyr::mutate(n_success = n_success,
                  na = n_na,
                  n = n,
                  se = se,
                  z = z,
                  p.value = c(base::ifelse(p.value < 0.0001,
                                        "< 0.0001",
                                        base::format.pval(p.value, digits = digits)))
    ) %>%
    dplyr::select(n_success, na, n, estimate, se, z, p.value) %>%
    finalize_tbl(digits = digits,
                 caption = caption,
                 striped = FALSE) %>%
    flextable::set_header_labels(n_success = "n\nSuccesses", na = "n\nMissing", n = "n\nUsed",
                                 estimate = "p\u0302", se = "Standard\nError", pval = "p-value") %>%
    fit_tbl()

}


#' Create a summary table for a two-sample proportion interval
#'
#' @inheritParams infer_1prop_int
#' @param formula The variables to run the test on, in formula syntax, `var1 ~ var2`.
#'
#' @return An object of class flextable. In an interactive environment, results are viewable immediately.
#' @export
#'
#' @examples
#' infer_2prop_int(mtcars, vs~am, success = 1)
#' infer_2prop_int(mtcars, vs~am, success = 1, conf_lvl = .9)
infer_2prop_int <- function(data, formula, success, digits = 3, conf_lvl = 0.95, caption = NULL) {

  # check for empty strings and make them actual NAs
  data <- tibble::as_tibble(data) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., "")))

  # error catching
  check_conf_lvl(conf_lvl)

  if (base::is.null(success)) {

    cli::cli_abort("A value for what a success is in this situation must be provided.")

  }

  var1 <- formula[[2]]
  var1_str <- base::deparse(base::substitute(var1))

  grp_var <- formula[[3]]
  grp_str <- base::deparse(base::substitute(grp_var))

  check_test(mosaic::prop.test(formula, data = data, conf.level = conf_lvl, success = success, correct = FALSE))

  # code
  two_prop <- mosaic::prop.test(formula, data = data, conf.level = conf_lvl, success = success, correct = FALSE)
  cl <- base::paste0(conf_lvl*100, "%")
  # build caption
  if (base::is.null(caption)) {

    caption <- base::paste("Two Sample Proportion Interval Between", var1_str, "and", grp_str,
                           "\nSuccesses:", success,
                           "\nConfidence Level:", cl)

  } else {

    caption <- base::paste(caption, "\nSuccesses:", success,
                           "\nConfidence Level:", cl)

  }



  grp_lvls <- sort(unique(dplyr::pull(data, grp_var)))

  # find NAs
  na1 <- data %>%
    dplyr::filter({{ grp_var }} == grp_lvls[1] & base::is.na({{ grp_var }})) %>%
    base::nrow()
  na2 <- data %>%
    dplyr::filter({{ grp_var }} == grp_lvls[2] & base::is.na({{ grp_var }})) %>%
    base::nrow()

  # find sample sizes
  n1 <- data %>%
    dplyr::filter({{ grp_var }} == grp_lvls[1]) %>%
    base::nrow()
  n2 <- data %>%
    dplyr::filter({{ grp_var }} == grp_lvls[2]) %>%
    base::nrow()

  # find success
  yay1 <- data %>%
    dplyr::select({{ var1 }}, {{ grp_var }}) %>%
    dplyr::filter({{ var1 }} == success & {{ grp_var }} == grp_lvls[1]) %>%
    mosaic::tally()
  yay2 <- data %>%
    dplyr::select({{ var1 }}, {{ grp_var }}) %>%
    dplyr::filter({{ var1 }} == success & {{ grp_var }} == grp_lvls[2]) %>%
    mosaic::tally()

  # manual calculations for STA 215

  se1 <- (two_prop$estimate[[1]]*(1 - two_prop$estimate[[1]])) / n1
  se2 <- (two_prop$estimate[[2]]*(1 - two_prop$estimate[[2]])) / n2
  se <- sqrt(se1 + se2)
  z_star <- round(stats::qnorm((1 - conf_lvl) / 2, lower.tail = FALSE), 3)
  moe <- se*z_star

  # build actual table
  tibble::tibble(
    var = base::as.character(grp_lvls),
    yay = c(yay1$n, yay2$n),
    n = c(n1, n2),
    na = c(na1, na2),
    phat = c(two_prop$estimate[[1]], two_prop$estimate[[2]]),
    se = c(se, NA),
    cil = c((two_prop$estimate[[1]] - two_prop$estimate[[2]]) - moe, NA),
    ciu = c((two_prop$estimate[[1]] - two_prop$estimate[[2]]) + moe, NA),
  ) %>%
    finalize_tbl(digits = digits,
                 caption = caption,
                 na_str = "") %>%
    flextable::set_header_labels(var = grp_str, yay = "n\nSuccesses", na = "n\nMissing", phat = "p\u0302",
                                 se = "Standard\nError", cil = base::paste(cl, "\nInterval\nLower"),
                                 ciu = base::paste(cl, "\nInterval\nUpper")) %>%
    flextable::vline(j = 5, border = officer::fp_border(width = 2)) %>%
    flextable::merge_at(i = 1:2, j = 6) %>%
    flextable::merge_at(i = 1:2, j = 7) %>%
    flextable::merge_at(i = 1:2, j = 8) %>%
    flextable::align(j = 6:8, align = "center") %>%
    fit_tbl()

}

#' Create a summary table for a two-sample proportion test
#'
#' @inheritParams infer_2prop_int
#' @param alternative The alternative hypothesis. Defaults to "notequal" (two sided p-value).
#'    Other options include "greater" or "less". Use depends on your test.
#'
#' @return An object of class flextable. In an interactive environment, results are viewable immediately.
#' @export
#'
#' @examples
#' infer_2prop_test(mtcars, vs~am, success = 1)
#' infer_2prop_test(mtcars, vs~am, success = 1, alternative = "less")
#' infer_2prop_test(mtcars, vs~am, success = 1, conf_lvl = .9, digits = 4)
infer_2prop_test <- function(data, formula, success, digits = 3, conf_lvl = 0.95,
                             alternative = c("notequal", "greater", "less"), caption = NULL) {

  # check for empty strings and make them actual NAs
  data <- tibble::as_tibble(data) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., "")))

  # error catching
  check_conf_lvl(conf_lvl)

  alternative = base::match.arg(alternative)

  if (alternative == "notequal") {
    alt_hyp <- "two.sided"
    p_header <- "Two Sided"
  } else if (alternative == "greater") {
    alt_hyp <- alternative
    p_header <- "One Sided (Greater Than)"
  } else {
    alt_hyp <- alternative
    p_header <- "One Sided (Less Than)"
  }

  check_test(mosaic::prop.test(formula, data = data, conf.level = conf_lvl,
                               alternative = alt_hyp,
                               success = success, correct = FALSE))


  # code
  two_prop <- mosaic::prop.test(formula, data = data, conf.level = conf_lvl,
                                alternative = alt_hyp,
                                success = success, correct = FALSE)

  var1 <- formula[[2]]
  var1_str <- base::deparse(base::substitute(var1))

  grp_var <- formula[[3]]
  grp_str <- base::deparse(base::substitute(grp_var))

  cl <- base::paste0(conf_lvl*100, "%")

  # build caption
  if (base::is.null(caption)) {

    caption <- base::paste("Two Sample Proportion Test Between", var1_str, "and", grp_str,
                           "\nSuccesses:", success,
                           "\np-value Reported:", p_header)

  } else {

    caption <- base::paste(caption, "\nSuccesses:", success,
                           "\np-value Reported:", p_header)

  }

  grp_lvls <- sort(unique(dplyr::pull(data, grp_var)))

  # find NAs
  na1 <- data %>%
    dplyr::filter({{ grp_var }} == grp_lvls[1] & base::is.na({{ grp_var }})) %>%
    base::nrow()
  na2 <- data %>%
    dplyr::filter({{ grp_var }} == grp_lvls[2] & base::is.na({{ grp_var }})) %>%
    base::nrow()

  # find sample sizes
  n1 <- data %>%
    dplyr::filter({{ grp_var }} == grp_lvls[1]) %>%
    base::nrow()
  n2 <- data %>%
    dplyr::filter({{ grp_var }} == grp_lvls[2]) %>%
    base::nrow()

  # find successes
  yay1 <- data %>%
    dplyr::select({{ var1 }}, {{ grp_var }}) %>%
    dplyr::filter({{ var1 }} == success & {{ grp_var }} == grp_lvls[1]) %>%
    mosaic::tally()
  yay2 <- data %>%
    dplyr::select({{ var1 }}, {{ grp_var }}) %>%
    dplyr::filter({{ var1 }} == success & {{ grp_var }} == grp_lvls[2]) %>%
    mosaic::tally()

  se1 <- (two_prop$estimate[[1]]*(1 - two_prop$estimate[[1]])) / n1
  se2 <- (two_prop$estimate[[2]]*(1 - two_prop$estimate[[2]])) / n2
  se <- sqrt(se1 + se2)

  # build table
  # tk check z, cil, and ciu (and point estimate)
  tibble::tibble(
    var = base::as.character(grp_lvls),
    yay = c(yay1$n, yay2$n),
    n = c(n1, n2),
    na = c(na1, na2),
    phat = c(two_prop$estimate[[1]], two_prop$estimate[[2]]),
    se = c(se, NA),
    z = c(two_prop$statistic, NA),
    p = c(base::ifelse(two_prop$p.value < 0.0001,
                       "< 0.0001",
                       base::format.pval(two_prop$p.value, digits = digits)), NA)
  ) %>%
    finalize_tbl(digits = digits,
                 caption = caption,
                 na_str = "") %>%
    flextable::set_header_labels(var = "Variable", yay = "n\nSuccesses", na = "n\nMissing", phat = "p\u0302",
                                 se = "Standard\nError", p = "p-value") %>%
    flextable::vline(j = 5, border = officer::fp_border(width = 2)) %>%
    flextable::merge_at(i = 1:2, j = 6) %>%
    flextable::merge_at(i = 1:2, j = 7) %>%
    flextable::merge_at(i = 1:2, j = 8) %>%
    flextable::align(j = 6:8, align = "center") %>%
    fit_tbl()

}
