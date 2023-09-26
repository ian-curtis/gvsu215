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
#' infer_1prop(mtcars, ~vs, success = 1)
#' infer_1prop(mtcars, ~vs, success = 1, conf_lvl = 0.90)
infer_1prop <- function(data, formula, success = NULL, digits = 3, conf_lvl = 0.95, caption = NULL) {

  # error catching
  check_conf_lvl(conf_lvl)

  if (base::is.null(success)) {

    stop("A value for what a success is in this situation must be provided.")

  }

  check_test(mosaic::prop.test(formula, data = data, conf.level = conf_lvl, success = success))

  #code

  var <- formula[[2]]
  var_str <- base::deparse(base::substitute(var))

  if (base::is.null(caption)) {

    caption <- base::paste("One-Sample Proportion Confidence Interval on Variable", var_str,
                           "\n Success:", success)

  } else {

    caption <- base::paste(caption, "\n Success:", success)

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
    flextable::set_header_labels(n_success = "n Success", na = "n Missing",
                                 estimate = "p-hat", se = "Standard Error",
                                 conf.low = base::paste(cl, "Interval Lower"),
                                 conf.high = base::paste(cl, "Interval Upper"))

}


#' Create a summary table for a two-sample proportion interval
#'
#' @inheritParams infer_1prop
#' @param formula The variables to run the test on, in formula syntax, `var1 ~ var2`.
#'
#' @return An object of class flextable. In an interactive environment, results are viewable immediately.
#' @export
#'
#' @examples
#' infer_2prop_int(mtcars, vs~am, success = 1)
#' infer_2prop_int(mtcars, vs~am, success = 1, conf_lvl = .9)
infer_2prop_int <- function(data, formula, success, digits = 3, conf_lvl = 0.95, caption = NULL) {

  # error catching
  check_conf_lvl(conf_lvl)

  if (base::is.null(success)) {

    stop("A value for what a success is in this situation must be provided.")

  }

  var1 <- formula[[2]]
  var1_str <- base::deparse(base::substitute(var1))

  grp_var <- formula[[3]]
  grp_str <- base::deparse(base::substitute(grp_var))

  check_test(mosaic::prop.test(formula, data = data, conf.level = conf_lvl, success = success, correct = FALSE))

  # code
  two_prop <- mosaic::prop.test(formula, data = data, conf.level = conf_lvl, success = success, correct = FALSE)

  if (base::is.null(caption)) {

    caption <- base::paste("Two Sample Proportion Interval Between", var1_str, "and", grp_str,
                           "\n Success:", success)

  } else {

    caption <- base::paste(caption, "\n Success:", success)

  }

  cl <- base::paste0(conf_lvl*100, "%")

  grp_lvls <- sort(unique(dplyr::pull(data, grp_var)))

  na1 <- data %>%
    dplyr::filter({{ grp_var }} == grp_lvls[1] & base::is.na({{ grp_var }})) %>%
    base::nrow()
  na2 <- data %>%
    dplyr::filter({{ grp_var }} == grp_lvls[2] & base::is.na({{ grp_var }})) %>%
    base::nrow()

  n1 <- data %>%
    dplyr::filter({{ grp_var }} == grp_lvls[1]) %>%
    base::nrow()
  n2 <- data %>%
    dplyr::filter({{ grp_var }} == grp_lvls[2]) %>%
    base::nrow()

  yay1 <- data %>%
    dplyr::select({{ var1 }}, {{ grp_var }}) %>%
    dplyr::filter({{ var1 }} == success & {{ grp_var }} == grp_lvls[1]) %>%
    mosaic::tally()
  yay2 <- data %>%
    dplyr::select({{ var1 }}, {{ grp_var }}) %>%
    dplyr::filter({{ var1 }} == success & {{ grp_var }} == grp_lvls[2]) %>%
    mosaic::tally()

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
    flextable::set_header_labels(var = grp_str, yay = "n Success", na = "n Missing", phat = "p-hat",
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

  # error catching
  check_conf_lvl(conf_lvl)

  check_test(mosaic::prop.test(formula, data = data, conf.level = conf_lvl, success = success, correct = FALSE))


  # code
  two_prop <- mosaic::prop.test(formula, data = data, conf.level = conf_lvl, success = success, correct = FALSE)

  var1 <- formula[[2]]
  var1_str <- base::deparse(base::substitute(var1))

  grp_var <- formula[[3]]
  grp_str <- base::deparse(base::substitute(grp_var))

  cl <- base::paste0(conf_lvl*100, "%")

  if (base::is.null(caption)) {

    caption <- base::paste("Two Sample Proportion Test Between", var1_str, "and", grp_str,
                           "\n Success:", success, "| Confidence:", cl)

  } else {

    caption <- base::paste(caption, "\n Success:", success, "| Confidence:", cl)

  }

  grp_lvls <- sort(unique(dplyr::pull(data, grp_var)))

  na1 <- data %>%
    dplyr::filter({{ grp_var }} == grp_lvls[1] & base::is.na({{ grp_var }})) %>%
    base::nrow()
  na2 <- data %>%
    dplyr::filter({{ grp_var }} == grp_lvls[2] & base::is.na({{ grp_var }})) %>%
    base::nrow()

  n1 <- data %>%
    dplyr::filter({{ grp_var }} == grp_lvls[1]) %>%
    base::nrow()
  n2 <- data %>%
    dplyr::filter({{ grp_var }} == grp_lvls[2]) %>%
    base::nrow()

  yay1 <- data %>%
    dplyr::select({{ var1 }}, {{ grp_var }}) %>%
    dplyr::filter({{ var1 }} == success & {{ grp_var }} == grp_lvls[1]) %>%
    mosaic::tally()
  yay2 <- data %>%
    dplyr::select({{ var1 }}, {{ grp_var }}) %>%
    dplyr::filter({{ var1 }} == success & {{ grp_var }} == grp_lvls[2]) %>%
    mosaic::tally()

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
    flextable::set_header_labels(var = "Variable", yay = "n Success", na = "n Missing", phat = "p-hat",
                                 se = "Standard Error", p = "p-value") %>%
    flextable::vline(j = 5)

}


#' Create a summary table for a one-sample mean interval
#'
#' @inheritParams infer_1prop
#'
#' @return An object of class flextable. In an interactive environment, results are viewable immediately.
#' @export
#'
#' @examples
#' infer_1mean(mtcars, ~wt)
#' infer_1mean(mtcars, ~wt, conf_lvl = .9)
infer_1mean <- function(data, formula, digits = 3, conf_lvl = 0.95, caption = NULL) {

  # error catching
  check_conf_lvl(conf_lvl)

  check_test(mosaic::t_test(formula, data = data, conf.level = conf_lvl))

  # code
  var <- formula[[2]]
  var_str <- base::deparse(base::substitute(var))

  if (base::is.null(caption)) {

    caption <- base::paste("One-Sample Mean Confidence Interval on Variable", var_str)

  }

  mu_test <- mosaic::t_test(formula, data = data, conf.level = conf_lvl)

  cl <-  base::paste0(conf_lvl*100, "%")

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
    flextable::set_header_labels(na = "n Misssing", estimate = "x-bar", se = "Standard Error",
                                 cil = base::paste(cl, "Interval Lower"),
                                 ciu = base::paste(cl, "Interval Upper"))

}


#' Create a summary table for a paired means test
#'
#' @inheritParams infer_1mean
#' @param var1 The first variable of the pair, entered in formula syntax `var1`.
#' @param var2 The second variable of the pair, entered in formula syntax `var2`.
#'
#' @return An object of class flextable. In an interactive environment, results are viewable immediately.
#' @export
#'
#' @examples
#' infer_paired(mtcars, var1 = ~drat, var2 = ~wt)
#' infer_paired(mtcars, var1 = ~drat, var2 = ~wt, conf_lvl = 0.9)
infer_paired <- function(data, var1, var2, digits = 3, conf_lvl = 0.95, caption = NULL) {

  # error catching

  var1 <- var1[[2]]
  var1_str <- base::deparse(base::substitute(var1))

  var2 <- var2[[2]]
  var_str2 <- base::deparse(base::substitute(var2))

  check_conf_lvl(conf_lvl)

  data <- data %>%
    dplyr::mutate(difference = {{ var1 }} - {{ var2 }})

  check_test(mosaic::t_test(formula = difference ~ 1, data = data, conf.level = conf_lvl))


  # code


  cl <- base::paste0(conf_lvl*100, "%")

  if (base::is.null(caption)) {

    caption <- base::paste("Difference in Means Test:", var1_str, "-", var_str2,
                           "\n", cl, "Confidence")

  } else {

    caption <- base::paste(caption, "\n", cl, "Confidence")

  }

  n_na <- find_na(data, ~ difference)

  n <- base::nrow(data) - n_na

  diff_t <- mosaic::t_test(formula = difference ~ 1, data = data, conf.level = conf_lvl)

  tibble::tibble(
    n = n,
    na = n_na,
    estimate = diff_t$estimate,
    se = diff_t$stderr,
    t = diff_t$statistic,
    df = as.integer(diff_t$parameter),
    p = ifelse(base::diff_t$p.value < 0.0001,
               "< 0.0001",
               format.pval(diff_t$p.value, digits = digits)),
    cil = diff_t$conf.int[[1]],
    ciu = diff_t$conf.int[[2]]
  ) %>%
    finalize_tbl(digits = digits, caption = caption, striped = FALSE) %>%
    flextable::set_header_labels(na = "n Missing", estimate = "x-bar",
                                 se = "Standard Error", p = "p-value",
                                 cil = base::paste(cl, "Interval Lower"),
                                 ciu = base::paste(cl, "Interval Upper"))

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

  # error catching
  check_conf_lvl(conf_lvl)

  var1 <- formula[[2]]
  var1_str <- base::deparse(base::substitute(var1))

  grp_var <- formula[[3]]
  grp_str <- base::deparse(base::substitute(grp_var))

  # error catching
  check_conf_lvl(conf_lvl)

  base::tryCatch(data %>% dplyr::mutate("{grp_var}" := base::factor({{ grp_var }})),
                 error = function (e) stop("Could not convert grouping variable into a factor. Perhaps you entered the grouping variable first (instead of second)?")
  )

  data <- data %>%
    dplyr::mutate("{grp_var}" := base::as.factor({{ grp_var }}))

  grp_lvls <- base::sort(base::levels(data[[grp_str]]))

  if (base::length(grp_lvls) != 2) {

    stop("The grouping variable must have two (and only two) levels. \n Perhaps you entered the grouping variable first (instead of second)?")

  }

  check_test(mosaic::t_test(formula, data = data, conf.level = conf_lvl))

  #code
  ind_test <- mosaic::t_test(formula, data = data, conf.level = conf_lvl)

  cl <- base::paste0(conf_lvl*100, "%")

  if (base::is.null(caption)) {

    caption <- base::paste("Two Sample Independent Means Interval Between", var1_str, "and", grp_str)

  } else {

    caption <- caption

  }

  na1 <- data %>%
    dplyr::filter({{ grp_var }} == grp_lvls[1] & base::is.na({{ grp_var }})) %>%
    base::nrow()
  na2 <- data %>%
    dplyr::filter({{ grp_var }} == grp_lvls[2] & base::is.na({{ grp_var }})) %>%
    base::nrow()

  n1 <- data %>%
    dplyr::filter({{ grp_var }} == grp_lvls[1]) %>%
    base::nrow()
  n2 <- data %>%
    dplyr::filter({{ grp_var }} == grp_lvls[2]) %>%
    base::nrow()

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
    flextable::set_header_labels(var = grp_str, na = "n Missing", xbar = "Group Means", se = "Standard Error",
                                 cil = base::paste(cl, "Interval Lower"),
                                 ciu = base::paste(cl, "Interval Upper")) %>%
    flextable::vline(j = c(3, 4))

}

#' Create a summary table for a two-sample mean test
#'
#' @inheritParams infer_2prop_int
#'
#' @return An object of class flextable. In an interactive environment, results are viewable immediately.
#' @export
#'
#' @examples
#' infer_2mean_test(mtcars, wt~vs)
#' infer_2mean_test(mtcars, wt~vs, conf_lvl = .9)
infer_2mean_test <- function(data, formula, digits = 3, conf_lvl = 0.95, caption = NULL) {

  # error catching
  check_conf_lvl(conf_lvl)

  var1 <- formula[[2]]
  var1_str <- base::deparse(base::substitute(var1))

  grp_var <- formula[[3]]
  grp_str <- base::deparse(base::substitute(grp_var))

  # error catching
  check_conf_lvl(conf_lvl)

  base::tryCatch(data %>% dplyr::mutate("{grp_var}" := base::factor({{ grp_var }})),
                 error = function (e) stop("Could not convert grouping variable into a factor. Perhaps you entered the grouping variable first (instead of second)?")
  )

  data <- data %>%
    dplyr::mutate("{grp_var}" := base::as.factor({{ grp_var }}))

  grp_lvls <- base::levels(data[[grp_str]])

  if (base::length(grp_lvls) != 2) {

    stop("The grouping variable must have two (and only two) levels. \n Perhaps you entered the grouping variable first (instead of second)?")

  }

  check_test(mosaic::t_test(formula, data = data, conf.level = conf_lvl))

  # code
  ind_test <- mosaic::t_test(formula, data = data, conf.level = conf_lvl)

  cl <- base::paste0(conf_lvl*100, "%")

  if (base::is.null(caption)) {

    caption <- base::paste("Two Sample Independent Means Test Between", var1_str, "and", grp_str,
                           "\n Confidence Level:", cl)

  } else {

    caption <- base::paste(caption, "\n Confidence Level:", cl)

  }

  na1 <- data %>%
    dplyr::filter({{ grp_var }} == grp_lvls[1] & base::is.na({{ grp_var }})) %>%
    base::nrow()
  na2 <- data %>%
    dplyr::filter({{ grp_var }} == grp_lvls[2] & base::is.na({{ grp_var }})) %>%
    base::nrow()

  n1 <- data %>%
    dplyr::filter({{ grp_var }} == grp_lvls[1]) %>%
    base::nrow()
  n2 <- data %>%
    dplyr::filter({{ grp_var }} == grp_lvls[2]) %>%
    base::nrow()

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
    flextable::set_header_labels(var = "Variable", na = "n Missing", s = "Group s",
                                 xbar = "Group Means",
                                 se = "Standard Error",
                                 p = "p-value") %>%
    flextable::vline(j = 4)

}
