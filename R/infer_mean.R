#' Create a summary table for a one-sample mean interval
#'
#' @inheritParams infer_1prop_int
#'
#' @return An object of class flextable. In an interactive environment, results are viewable immediately.
#' @export
#'
#' @examples
#' infer_1mean_int(mtcars, ~wt)
#' infer_1mean_int(mtcars, ~wt, conf_lvl = .9)
infer_1mean_int <- function(data, formula, digits = 3, conf_lvl = 0.95, caption = NULL) {

  # check for empty strings and make them actual NAs
  data <- tibble::as_tibble(data) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., "")))

  # error catching
  check_conf_lvl(conf_lvl)

  check_test(mosaic::t_test(formula, data = data, conf.level = conf_lvl))
  cl <-  base::paste0(conf_lvl*100, "%")
  # code
  var <- formula[[2]]
  var_str <- base::deparse(base::substitute(var))

  # build caption
  if (base::is.null(caption)) {

    caption <- base::paste("One-Sample Mean Interval on Variable", var_str,
                           "\nConfidence Level:", cl)

  } else {

    caption <- base::paste(caption,
                           "\nConfidence Level:", cl)

  }

  mu_test <- mosaic::t_test(formula, data = data, conf.level = conf_lvl)



  n_na <- find_na(data, formula)

  n <- base::nrow(data) - n_na

  tibble::tibble(
    n = n,
    na = n_na,
    estimate = mu_test$estimate,
    df = as.integer(mu_test$parameter),
    se = mu_test$stderr,
    cil = mu_test$conf.int[[1]],
    ciu = mu_test$conf.int[[2]]
  ) %>%
    finalize_tbl(digits = digits, caption = caption, striped = FALSE) %>%
    flextable::set_header_labels(na = "n\nMissing", estimate = "x\u0304", se = "Standard\nError",
                                 cil = base::paste(cl, "\nInterval\nLower"),
                                 ciu = base::paste(cl, "\nInterval\nUpper")) %>%
    fit_tbl()

}

#' Create a summary table for a one-sample mean hypothesis test
#'
#' @inheritParams infer_1mean_int
#' @param mu0 The null hypothesis value. Defaults to 0.
#' @param alternative The alternative hypothesis. Defaults to "notequal" (two sided p-value).
#'    Other options include "greater" or "less". Use depends on your test.
#'
#' @return An object of class flextable. In an interactive environment, results are viewable immediately.
#' @export
#'
#' @examples
#' infer_1mean_test(mtcars, ~wt)
#' infer_1mean_test(mtcars, ~wt, mu0 = 3)
#' infer_1mean_test(mtcars, ~wt, digits = 4, conf_lvl = 0.90)
infer_1mean_test <- function(data, formula, digits = 3, mu0 = 0,
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

  check_test(mosaic::t_test(formula, data = data, conf.level = conf_lvl, mu = mu0,
                            alternative = alt_hyp))

  # code
  var <- formula[[2]]
  var_str <- base::deparse(base::substitute(var))

  # build caption
  cl <-  base::paste0(conf_lvl*100, "%")

  if (base::is.null(caption)) {

    caption <- base::paste("One-Sample Mean Test on Variable", var_str,
                           "\nNull Value:", mu0,
                           "\np-value Reported:", p_header)

  }

  mu_test <- mosaic::t_test(formula, data = data, conf.level = conf_lvl, mu = mu0,
                            alternative = alt_hyp)

  n_na <- find_na(data, formula)

  n <- base::nrow(data) - n_na

  tibble::tibble(
    n = n,
    na = n_na,
    estimate = mu_test$estimate,
    se = mu_test$stderr,
    t = mu_test$statistic,
    df = as.integer(mu_test$parameter),
    p = c(base::ifelse(mu_test$p.value < 0.0001,
                       "< 0.0001",
                       base::format.pval(mu_test$p.value, digits = digits)))
  ) %>%
    finalize_tbl(digits = digits, caption = caption, striped = FALSE) %>%
    flextable::set_header_labels(n = "n Used", na = "n\nMissing", estimate = "x\u0304", se = "Standard\nError",
                                 df = "Degrees of\nFreedom", p = "p-value") %>%
    fit_tbl()

}


#' Create a summary table for a paired means test
#'
#' @inheritParams infer_1mean_int
#' @param var1 The first variable of the pair, entered in formula syntax `var1`.
#' @param var2 The second variable of the pair, entered in formula syntax `var2`.
#' @param mu0 The null hypothesis value. Defaults to 0.
#'
#' @return An object of class flextable. In an interactive environment, results are viewable immediately.
#' @export
#'
#' @examples
#' infer_paired(mtcars, var1 = ~drat, var2 = ~wt)
#' infer_paired(mtcars, var1 = ~drat, var2 = ~wt, conf_lvl = 0.9)
infer_paired <- function(data, var1, var2, digits = 3, mu0 = 0, conf_lvl = 0.95, caption = NULL) {

  # check for empty strings and make them actual NAs
  data <- tibble::as_tibble(data) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., "")))

  # error catching

  var1 <- var1[[2]]
  var1_str <- base::deparse(base::substitute(var1))

  var2 <- var2[[2]]
  var_str2 <- base::deparse(base::substitute(var2))

  check_conf_lvl(conf_lvl)

  data <- data %>%
    dplyr::mutate(difference = {{ var1 }} - {{ var2 }})

  check_test(mosaic::t_test(formula = difference ~ 1, data = data, conf.level = conf_lvl, mu = mu0))


  # code


  cl <- base::paste0(conf_lvl*100, "%")

  # build caption
  if (base::is.null(caption)) {

    caption <- base::paste("Difference in Means Test:", var1_str, "-", var_str2,
                           "\n", cl, "Confidence",
                           "\nNull Value:", mu0)

  } else {

    caption <- base::paste(caption, "\n", cl, "Confidence",
                           "\nNull Value:", mu0)

  }

  n_na <- find_na(data, ~ difference)

  n <- base::nrow(data) - n_na

  diff_t <- mosaic::t_test(formula = difference ~ 1, data = data, conf.level = conf_lvl, mu = mu0)

  # build table
  tibble::tibble(
    n = n,
    na = n_na,
    estimate = diff_t$estimate,
    se = diff_t$stderr,
    t = diff_t$statistic,
    df = as.integer(diff_t$parameter),
    p = ifelse(diff_t$p.value < 0.0001,
               "< 0.0001",
               format.pval(diff_t$p.value, digits = digits)),
    cil = diff_t$conf.int[[1]],
    ciu = diff_t$conf.int[[2]]
  ) %>%
    finalize_tbl(digits = digits, caption = caption, striped = FALSE) %>%
    flextable::set_header_labels(na = "n\nMissing", estimate = "x\u0304",
                                 se = "Standard\nError", p = "p-value\n(2 tail)",
                                 cil = base::paste(cl, "\nInterval\nLower"),
                                 ciu = base::paste(cl, "\nInterval\nUpper")) %>%
    fit_tbl()

}


#' Create a summary table of a two-sample mean interval
#'
#' @inheritParams infer_2prop_int
#'
#' @return An object of class flextable. In an interactive environment, results are viewable immediately.
#' @export
#'
#' @examples
#' infer_2mean_int(mtcars, wt~vs)
#' infer_2mean_int(mtcars, wt~vs, conf_lvl = .9)
infer_2mean_int <- function(data, formula, digits = 3, conf_lvl = 0.95, caption = NULL) {

  # check for empty strings and make them actual NAs
  data <- tibble::as_tibble(data) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., "")))

  # error catching
  check_conf_lvl(conf_lvl)

  var1 <- formula[[2]]
  var1_str <- base::deparse(base::substitute(var1))

  grp_var <- formula[[3]]
  grp_str <- base::deparse(base::substitute(grp_var))


  base::tryCatch(data %>% dplyr::mutate("{grp_var}" := base::factor({{ grp_var }})),
                 error = function (e) cli::cli_abort("Could not convert grouping variable into a factor. Perhaps you entered the grouping variable first (instead of second)?")
  )

  data <- data %>%
    dplyr::mutate("{grp_var}" := base::as.factor({{ grp_var }}))

  grp_lvls <- base::sort(base::levels(data[[grp_str]]))

  if (base::length(grp_lvls) != 2) {

    cli::cli_abort("The grouping variable must have two (and only two) levels.\nPerhaps you entered the grouping variable first (instead of second)?")

  }

  check_test(mosaic::t_test(formula, data = data, conf.level = conf_lvl))

  #code
  ind_test <- mosaic::t_test(formula, data = data, conf.level = conf_lvl)

  cl <- base::paste0(conf_lvl*100, "%")

  # build caption
  if (base::is.null(caption)) {

    caption <- base::paste("Two Sample Independent Means Interval Between", var1_str, "and", grp_str,
                           "\nConfidence Level:", cl)

  } else {

    caption <- base::paste(caption, "\nConfidence Level:", cl)

  }

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

  # build table
  tibble::tibble(
    var = base::as.character(grp_lvls),
    n = c(n1, n2),
    na = c(na1, na2),
    xbar = c(ind_test$estimate[[1]], ind_test$estimate[[2]]),
    se = c(ind_test$stderr, NA),
    df = c(ind_test$parameter, NA),
    cil = c(ind_test$conf.int[[1]], NA),
    ciu = c(ind_test$conf.int[[2]], NA)
  ) %>%
    finalize_tbl(digits = digits, caption = caption, na_str = "") %>%
    flextable::set_header_labels(var = grp_str, na = "n\nMissing", xbar = "Group\nMeans", se = "Standard\nError",
                                 cil = base::paste(cl, "\nInterval\nLower"),
                                 ciu = base::paste(cl, "\nInterval\nUpper")) %>%
    flextable::vline(j = 4, border = officer::fp_border(width = 2)) %>%
    flextable::merge_at(i = 1:2, j = 5) %>%
    flextable::merge_at(i = 1:2, j = 6) %>%
    flextable::merge_at(i = 1:2, j = 7) %>%
    flextable::merge_at(i = 1:2, j = 8) %>%
    flextable::align(j = 5:8, align = "center") %>%
    fit_tbl()

}

#' Create a summary table for a two-sample mean test
#'
#' @inheritParams infer_2prop_int
#' @param mu0 The null hypothesis value. Defaults to 0.
#' @param alternative The alternative hypothesis. Defaults to "notequal" (two sided p-value).
#'    Other options include "greater" or "less". Use depends on your test.
#'
#'
#' @return An object of class flextable. In an interactive environment, results are viewable immediately.
#' @export
#'
#' @examples
#' infer_2mean_test(mtcars, wt~vs)
#' infer_2mean_test(mtcars, wt~vs, conf_lvl = .9)
infer_2mean_test <- function(data, formula, digits = 3, mu0 = 0,
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

  var1 <- formula[[2]]
  var1_str <- base::deparse(base::substitute(var1))

  grp_var <- formula[[3]]
  grp_str <- base::deparse(base::substitute(grp_var))

  check_conf_lvl(conf_lvl)

  base::tryCatch(data %>% dplyr::mutate("{grp_var}" := base::factor({{ grp_var }})),
                 error = function (e) cli::cli_abort("Could not convert grouping variable into a factor. Perhaps you entered the grouping variable first (instead of second)?")
  )

  data <- data %>%
    dplyr::mutate("{grp_var}" := base::as.factor({{ grp_var }}))

  grp_lvls <- base::levels(data[[grp_str]])

  if (base::length(grp_lvls) != 2) {

    cli::cli_abort("The grouping variable must have two (and only two) levels.\nPerhaps you entered the grouping variable first (instead of second)?")

  }

  check_test(mosaic::t_test(formula, data = data, conf.level = conf_lvl, mu = mu0,
                            alternative = alt_hyp))

  # code
  ind_test <- mosaic::t_test(formula, data = data, conf.level = conf_lvl, mu = mu0,
                             alternative = alt_hyp)

  cl <- base::paste0(conf_lvl*100, "%")

  # build caption
  if (base::is.null(caption)) {

    caption <- base::paste("Two Sample Independent Means Test Between", var1_str, "and", grp_str,
                           "\nNull Hypothesis Value (Difference in Means):", mu0,
                           "\np-value Reported:", p_header)

  } else {

    caption <- base::paste(caption, "\nNull Hypothesis Value (Difference in Means):", mu0,
                           "\np-value Reported:", p_header)

  }

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

  # build table
  tibble::tibble(
    var = base::as.character(grp_lvls),
    n = c(n1, n2),
    na = c(na1, na2),
    xbar = c(ind_test$estimate[[1]], ind_test$estimate[[2]]),
    se = c(ind_test$stderr, NA),
    t = c(ind_test$statistic, NA),
    df = c(ind_test$parameter, NA),
    p = c(base::ifelse(ind_test$p.value < 0.0001,
                       "< 0.0001",
                       base::format.pval(ind_test$p.value, digits = digits)), NA)
  ) %>%
    finalize_tbl(digits = digits, caption = caption, na_str = "") %>%
    flextable::set_header_labels(var = "Variable", na = "n\nMissing", s = "Group s",
                                 xbar = "Group\nMeans",
                                 se = "Standard\nError",
                                 p = "p-value") %>%
    flextable::vline(j = 4, border = officer::fp_border(width = 2)) %>%
    flextable::merge_at(i = 1:2, j = 5) %>%
    flextable::merge_at(i = 1:2, j = 6) %>%
    flextable::merge_at(i = 1:2, j = 7) %>%
    flextable::merge_at(i = 1:2, j = 8) %>%
    flextable::align(j = 5:8, align = "center") %>%
    fit_tbl()

}
