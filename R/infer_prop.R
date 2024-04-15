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
infer_1prop_int <- function(data, formula, success = NULL, digits = 3, conf_lvl = 0.95, caption = NULL) {

  # check for empty strings and make them actual NAs
  data <- tibble::as_tibble(data) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., "")))

  # error catching
  check_conf_lvl(conf_lvl)

  if (base::is.null(success)) {

    cli::cli_abort("A value for what a success is in this situation must be provided.")

  }

  check_test(mosaic::prop.test(formula, data = data, conf.level = conf_lvl, success = success))

  #code

  var <- formula[[2]]
  var_str <- base::deparse(base::substitute(var))

  # build caption
  if (base::is.null(caption)) {

    caption <- base::paste("One-Sample Proportion Confidence Interval on Variable", var_str,
                           "\n Successes:", success)

  } else {

    caption <- base::paste(caption, "\n Successes:", success)

  }

  prop_test <- mosaic::prop.test(formula, data = data, conf.level = conf_lvl, success = success)

  cl <-  base::paste0(conf_lvl*100, "%")

  n_success <- data %>%
    dplyr::filter({{ var }} == success) %>%
    base::nrow()

  n_na <- find_na(data, formula)

  n <- base::nrow(data) - n_na

  broom::tidy(prop_test) %>%
    dplyr::mutate(n_success = n_success,
           na = n_na,
           n = n,
           se = base::sqrt((prop_test$estimate * (1 - prop_test$estimate)) / n)
    ) %>%
    dplyr::select(n_success, na, n, estimate, se, conf.low, conf.high) %>%
    finalize_tbl(digits = digits,
                 caption = caption,
                 striped = FALSE) %>%
    flextable::set_header_labels(n_success = "n Successes", na = "n Missing",
                                 estimate = "p\u0302", se = "Standard Error",
                                 conf.low = base::paste(cl, "Interval Lower"),
                                 conf.high = base::paste(cl, "Interval Upper"))

}

#' Create a summary table for a one-sample proportion test
#'
#' @inheritParams infer_1prop_int
#' @param p0 The null hypothesis value. Defaults to 0.5.
#'
#' @return An object of class flextable. In an interactive environment, results are viewable immediately.
#' @export
#'
#' @examples
#' infer_1prop_test(mtcars, ~vs, success = 1)
#' infer_1prop_test(mtcars, ~vs, success = 1, conf_lvl = 0.90)
#' infer_1prop_test(mtcars, ~vs, success = 1, p0 = 0.4)
infer_1prop_test <- function(data, formula, success = NULL, p0 = 0.5, digits = 3, conf_lvl = 0.95, caption = NULL) {

  # check for empty strings and make them actual NAs
  data <- tibble::as_tibble(data) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., "")))

  # error catching
  check_conf_lvl(conf_lvl)

  if (base::is.null(success)) {

    cli::cli_abort("A value for what a success is in this situation must be provided.")

  }

  check_test(mosaic::prop.test(formula, data = data, p = p0, conf.level = conf_lvl, success = success))

  #code

  var <- formula[[2]]
  var_str <- base::deparse(base::substitute(var))

  # build caption
  if (base::is.null(caption)) {

    caption <- base::paste("One-Sample Proportion Test on Variable", var_str,
                           "\n Successes:", success,
                           "\n Null Value:", p0)

  } else {

    caption <- base::paste(caption, "\n Success:", success,
                           "\n Null Value:", p0)

  }

  prop_test <- mosaic::prop.test(formula, data = data, p = p0, conf.level = conf_lvl, success = success)

  cl <-  base::paste0(conf_lvl*100, "%")

  n_success <- data %>%
    dplyr::filter({{ var }} == success) %>%
    base::nrow()

  n_na <- find_na(data, formula)

  n <- base::nrow(data) - n_na

  broom::tidy(prop_test) %>%
    dplyr::mutate(n_success = n_success,
                  na = n_na,
                  n = n,
                  se = base::sqrt((prop_test$estimate * (1 - prop_test$estimate)) / n)
    ) %>%
    dplyr::select(n_success, na, n, estimate, se, statistic, p.value) %>%
    finalize_tbl(digits = digits,
                 caption = caption,
                 striped = FALSE) %>%
    flextable::set_header_labels(n_success = "n Successes", na = "n Missing", n = "n Used",
                                 estimate = "p\u0302", se = "Standard Error",
                                 statistic = "z", p.value = "p-value")

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

  # build caption
  if (base::is.null(caption)) {

    caption <- base::paste("Two Sample Proportion Interval Between", var1_str, "and", grp_str,
                           "\n Successes:", success)

  } else {

    caption <- base::paste(caption, "\n Successes:", success)

  }

  cl <- base::paste0(conf_lvl*100, "%")

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

  # build actual table
  tibble::tibble(
    var = base::as.character(grp_lvls),
    yay = c(yay1$n, yay2$n),
    n = c(n1, n2),
    na = c(na1, na2),
    phat = c(two_prop$estimate[[1]], two_prop$estimate[[2]]),
    se = c(sqrt((two_prop$estimate[[1]]*(1-two_prop$estimate[[1]])/n1) + (two_prop$estimate[[2]]*(1-two_prop$estimate[[2]])/n2)), NA),
    cil = c(two_prop$conf.int[[1]], NA),
    ciu = c(two_prop$conf.int[[2]], NA)
  ) %>%
    finalize_tbl(digits = digits,
                 caption = caption,
                 na_str = "") %>%
    flextable::set_header_labels(var = grp_str, yay = "n Successes", na = "n Missing", phat = "p\u0302",
                                 se = "Standard Error", cil = base::paste(cl, "Interval Lower"),
                                 ciu = base::paste(cl, "Interval Upper")) %>%
    flextable::vline(j = 5)

}

#' Create a summary table for a two-sample proportion test
#'
#' @inheritParams infer_2prop_int
#'
#' @return An object of class flextable. In an interactive environment, results are viewable immediately.
#' @export
#'
#' @examples
#' infer_2prop_test(mtcars, vs~am, success = 1)
#' infer_2prop_test(mtcars, vs~am, success = 1, conf_lvl = .9, digits = 4)
infer_2prop_test <- function(data, formula, success, digits = 3, conf_lvl = 0.95, caption = NULL) {

  # check for empty strings and make them actual NAs
  data <- tibble::as_tibble(data) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., "")))

  # error catching
  check_conf_lvl(conf_lvl)

  print(mosaic::prop.test(formula, data = data, conf.level = conf_lvl, success = success, correct = FALSE))


  # code
  two_prop <- mosaic::prop.test(formula, data = data, conf.level = conf_lvl, success = success, correct = FALSE)

  var1 <- formula[[2]]
  var1_str <- base::deparse(base::substitute(var1))

  grp_var <- formula[[3]]
  grp_str <- base::deparse(base::substitute(grp_var))

  cl <- base::paste0(conf_lvl*100, "%")

  # build caption
  if (base::is.null(caption)) {

    caption <- base::paste("Two Sample Proportion Test Between", var1_str, "and", grp_str,
                           "\n Successes:", success, "| Confidence:", cl)

  } else {

    caption <- base::paste(caption, "\n Successes:", success, "| Confidence:", cl)

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

  # build table
  tibble::tibble(
    var = base::as.character(grp_lvls),
    yay = c(yay1$n, yay2$n),
    n = c(n1, n2),
    na = c(na1, na2),
    phat = c(two_prop$estimate[[1]], two_prop$estimate[[2]]),
    se = c(sqrt((two_prop$estimate[[1]]*(1-two_prop$estimate[[1]])/n1) + (two_prop$estimate[[2]]*(1-two_prop$estimate[[2]])/n2)), NA),
    z = c(two_prop$statistic, NA),
    p = c(base::ifelse(two_prop$p.value < 0.0001,
                 "< 0.0001",
                 base::format.pval(two_prop$p.value, digits = digits)), NA)
  ) %>%
    finalize_tbl(digits = digits,
                 caption = caption,
                 na_str = "") %>%
    flextable::set_header_labels(var = "Variable", yay = "n Successes", na = "n Missing", phat = "p\u0302",
                                 se = "Standard Error", p = "p-value") %>%
    flextable::vline(j = 5)

}


