infer_prop1 <- function(data, formula, success = NULL, digits = 3, conf_lvl = 0.95, caption = NULL) {

  # error catching
  check_conf_lvl(conf_lvl)

  if (base::is.null(success)) {

    stop("A value for what a success is in this situation must be provided.")

  }

  check_test(mosaic::prop.test(formula, data = data, conf.level = conf_lvl, success = success))

  #code

  var <- formula[[2]]
  str_of_var <- base::deparse(base::substitute(var))

  if (base::is.null(caption)) {

    caption <- base::paste("One-Sample Proportion Confidence Interval on Variable", str_of_var,
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
    # kbl(digits = 3,
    #     col.names = c("Success", "n Success", "n", "p-hat", "Standard Error", "Confidence Level", "Lower", "Upper")) %>%
    # kable_styling(c('condensed', 'bordered'), full_width = F) %>%
    # add_header_above(c("", "", "", "", "", "", "Confidence Interval" = 2)) %>%
    # row_spec(0, extra_css = "border-bottom: 3px solid black")

}


infer_prop2_int <- function(data, formula, success, digits = 3, conf_lvl = 0.95, caption = NULL) {

  # error catching
  check_conf_lvl(conf_lvl)

  if (base::is.null(success)) {

    stop("A value for what a success is in this situation must be provided.")

  }

  check_test(mosaic::prop.test(formula, data = data, conf.level = conf_lvl, success = success, correct = FALSE))

  # code
  two_prop <- mosaic::prop.test(formula, data = data, conf.level = conf_lvl, success = success, correct = FALSE)

  var1 <- formula[[2]]
  str_of_var1 <- base::deparse(base::substitute(var1))

  var2 <- formula[[3]]
  str_of_var2 <- base::deparse(base::substitute(var2))

  if (base::is.null(caption)) {

    caption <- base::paste("Two Sample Proportion Interval Between", str_of_var1, "and", str_of_var2,
                           "\n Success:", success)

  } else {

    caption <- base::paste(caption, "\n Success:", success)

  }

  cl <- base::paste0(conf_lvl*100, "%")

  n_na <- find_na(data, formula, n = 2)

  n1 <- base::nrow(data) - n_na[[1]]
  n2 <- base::nrow(data) - n_na[[2]]

  yay1 <- data %>%
    dplyr::select({{ var1 }}) %>%
    dplyr::filter({{ var1 }} == success) %>%
    mosaic::tally()
  yay2 <- data %>%
    dplyr::select({{ var2 }}) %>%
    dplyr::filter({{ var2 }} == success) %>%
    mosaic::tally()

  tibble::tibble(
    var = c(str_of_var1, str_of_var2),
    yay = c(yay1$n, yay2$n),
    n = c(n1, n2),
    na = c(n_na[[1]], n_na[[2]]),
    phat = c(two_prop$estimate[[1]], two_prop$estimate[[2]]),
    se = c(sqrt((two_prop$estimate[[1]]*(1-two_prop$estimate[[1]])/n1) + (two_prop$estimate[[2]]*(1-two_prop$estimate[[2]])/n2)), NA),
    cil = c(two_prop$conf.int[[1]], NA),
    ciu = c(two_prop$conf.int[[2]], NA)
  ) %>%
    finalize_tbl(digits = digits,
                 caption = caption,
                 striped = FALSE,
                 na_str = "") %>%
    flextable::set_header_labels(var = "Variable", yay = "n Success", na = "n Missing", phat = "p-hat",
                                 se = "Standard Error", cil = base::paste(cl, "Interval Lower"),
                                 ciu = base::paste(cl, "Interval Upper")) %>%
    flextable::vline(j = 5)


    # kbl(digits = 3,
    #     col.names = c("n1", "n2", "Success 1", "Success 2", "p-hat 1", "p-hat 2", "z", "Standard \n Error",
    #                   "Confidence \n Level", "Lower", "Upper")) %>%
    # kable_styling(c('condensed', 'bordered', 'striped'), full_width = F) %>%
    # add_header_above(c("", "", "", "", "", "", "", "", "", "Confidence Interval" = 2)) %>%
    # row_spec(0, extra_css = "border-bottom: 3px solid black")

}

infer_prop2_test <- function(data, formula, success, digits = 3, conf_lvl = 0.95, caption = NULL) {

  # error catching
  check_conf_lvl(conf_lvl)

  check_test(mosaic::prop.test(formula, data = data, conf.level = conf_lvl, success = success, correct = FALSE))


  # code
  two_prop <- mosaic::prop.test(formula, data = data, conf.level = conf_lvl, success = success, correct = FALSE)

  var1 <- formula[[2]]
  str_of_var1 <- base::deparse(base::substitute(var1))

  var2 <- formula[[3]]
  str_of_var2 <- base::deparse(base::substitute(var2))

  cl <- base::paste0(conf_lvl*100, "%")

  if (base::is.null(caption)) {

    caption <- base::paste("Two Sample Proportion Test Between", str_of_var1, "and", str_of_var2,
                           "\n Success:", success, "| Confidence:", cl)

  } else {

    caption <- base::paste(caption, "\n Success:", success, "| Confidence:", cl)

  }

  n_na <- find_na(data, formula, n = 2)

  n1 <- base::nrow(data) - n_na[[1]]
  n2 <- base::nrow(data) - n_na[[2]]

  yay1 <- data %>%
    dplyr::select({{ var1 }}) %>%
    dplyr::filter({{ var1 }} == success) %>%
    mosaic::tally()
  yay2 <- data %>%
    dplyr::select({{ var2 }}) %>%
    dplyr::filter({{ var2 }} == success) %>%
    mosaic::tally()

  tibble::tibble(
    var = c(str_of_var1, str_of_var2),
    yay = c(yay1$n, yay2$n),
    n = c(n1, n2),
    na = c(n_na[[1]], n_na[[2]]),
    phat = c(two_prop$estimate[[1]], two_prop$estimate[[2]]),
    se = c(sqrt((two_prop$estimate[[1]]*(1-two_prop$estimate[[1]])/n1) + (two_prop$estimate[[2]]*(1-two_prop$estimate[[2]])/n2)), NA),
    z = c(two_prop$statistic, NA),
    p = c(base::format.pval(two_prop$p.value, digits = digits), NA)
  ) %>%
    finalize_tbl(digits = digits,
                 caption = caption,
                 striped = FALSE,
                 na_str = "") %>%
    flextable::set_header_labels(var = "Variable", yay = "n Success", na = "n Missing", phat = "p-hat",
                                 se = "Standard Error", p = "p-value") %>%
    flextable::vline(j = 5)

}


infer_mean1 <- function(data, formula, digits = 3, conf_lvl = 0.95, caption = NULL) {

  # error catching
  check_conf_lvl(conf_lvl)

  check_test(mosaic::t_test(formula, data = data, conf.level = conf_lvl))

  # code
  var <- formula[[2]]
  str_of_var <- base::deparse(base::substitute(var))

  if (base::is.null(caption)) {

    caption <- base::paste("One-Sample Mean Confidence Interval on Variable", str_of_var)

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

    # kbl(digits = 3,
    #     col.names = c("Var", "n", "x-bar", "df", "Standard Error", "Confidence Level", "Lower", "Upper")) %>%
    # kable_styling(c('condensed', 'bordered'), full_width = F) %>%
    # add_header_above(c( "", "", "", "", "", "", "Confidence Interval" = 2)) %>%
    # row_spec(0, extra_css = "border-bottom: 3px solid black")

}


infer_paired <- function(data, var1, var2, digits = 3, conf_lvl = 0.95, caption = NULL) {

  # error catching

  left_var <- var1[[2]]
  str_of_var1 <- base::deparse(base::substitute(left_var))

  right_var <- var2[[2]]
  str_of_var2 <- base::deparse(base::substitute(right_var))

  check_conf_lvl(conf_lvl)

  data <- data %>%
    dplyr::mutate(difference = {{ left_var }} - {{ right_var }})

  check_test(mosaic::t_test(formula = difference ~ 1, data = data, conf.level = conf_lvl))


  # code


  cl <- base::paste0(conf_lvl*100, "%")

  if (base::is.null(caption)) {

    caption <- base::paste("Difference in Means Test:", str_of_var1, "-", str_of_var2,
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
    sd = "TBD",
    se = diff_t$stderr,
    t = diff_t$statistic,
    df = as.integer(diff_t$parameter),
    p = format.pval(diff_t$p.value, digits = digits)
  ) %>%
    finalize_tbl(digits = digits, caption = caption, striped = FALSE) %>%
    flextable::set_header_labels(na = "n Missing", estimate = "x-bar", sd = "Standard Deviation",
                                 se = "Standard Error", p = "p-value")


  # kbl(digits = 3,
  #     caption = paste("Difference in Means Test: <br>", "Var1", "-", "Var2"),
  #     col.names = c("Estimate", "Standard Deviation", "Standard Error", "t", "df", "p-value")) %>%
  # kable_styling(c('condensed', 'bordered', 'striped'), full_width = F)

}


infer_mean2_int <- function(data, formula, digits = 3, conf_lvl = 0.95, caption = NULL) {

  # error catching
  check_conf_lvl(conf_lvl)

  var1 <- formula[[2]]
  str_of_var1 <- base::deparse(base::substitute(var1))

  var2 <- formula[[3]]
  str_of_var2 <- base::deparse(base::substitute(var2))

  # error catching
  check_conf_lvl(conf_lvl)

  base::tryCatch(data %>% dplyr::mutate("{var2}" := base::factor({{ var2 }})),
                 error = function (e) stop("Could not convert grouping variable into a factor. Perhaps you entered the grouping variable first (instead of second)?")
  )

  data <- data %>%
    dplyr::mutate("{var2}" := base::as.factor({{ var2 }}))

  fctr_lvls <- base::levels(data[[str_of_var2]])

  if (base::length(fctr_lvls) != 2) {

    stop("The grouping variable must have two (and only two) levels. \n Perhaps you entered the grouping variable first (instead of second)?")

  }

  check_test(mosaic::t_test(formula, data = data, conf.level = conf_lvl))

  #code
  ind_test <- mosaic::t_test(formula, data = data, conf.level = conf_lvl)

  cl <- base::paste0(conf_lvl*100, "%")

  data <- data %>%
    dplyr::mutate("{var2}" := base::factor({{ var2 }}))

  if (base::is.null(caption)) {

    caption <- base::paste("Two Sample Independent Means Interval Between", str_of_var1, "and", str_of_var2)

  } else {

    caption <- caption

  }

  n_na <- find_na(data, formula, n = 2)

  n1 <- base::nrow(data) - n_na[[1]]
  n2 <- base::nrow(data) - n_na[[2]]

  tibble::tibble(
    var = c(str_of_var1, str_of_var2),
    n = c(n1, n2),
    na = c(n_na[[1]], n_na[[2]]),
    xbar = c(ind_test$estimate[[1]], ind_test$estimate[[2]]),
    se = c(ind_test$stderr, NA),
    df = c(ind_test$parameter, NA),
    cil = c(ind_test$conf.int[[1]], NA),
    ciu = c(ind_test$conf.int[[2]], NA)
  ) %>%
    finalize_tbl(digits = digits, caption = caption, na_str = "") %>%
    flextable::set_header_labels(var = "Variable", na = "n Missing", xbar = "Group Means", se = "Standard Error",
                                 cil = base::paste(cl, "Interval Lower"),
                                 ciu = base::paste(cl, "Interval Upper")) %>%
    flextable::vline(j = c(3, 4))

  #   kbl(digits = 3,
  #       col.names = c("x-bar\u2081", "x-bar2\u2082", "n\u2081", "n\u2082", "sd\u2081", "sd\u2082", "x-bar Difference", "Standard Error", "df", "Confidence Level", "Lower", "Upper")) %>%
  #   kable_styling(c('condensed', 'bordered', 'striped'), full_width = F) %>%
  #   add_header_above(c("", "", "", "", "", "", "", "", "", "", "Confidene Interval" = 2)) %>%
  #   row_spec(0, extra_css = "border-bottom: 3px solid black")

}

infer_mean2_test <- function(data, formula, digits = 3, conf_lvl = 0.95, caption = NULL) {

  # error catching
  check_conf_lvl(conf_lvl)

  var1 <- formula[[2]]
  str_of_var1 <- base::deparse(base::substitute(var1))

  var2 <- formula[[3]]
  str_of_var2 <- base::deparse(base::substitute(var2))

  # error catching
  check_conf_lvl(conf_lvl)

  base::tryCatch(data %>% dplyr::mutate("{var2}" := base::factor({{ var2 }})),
                 error = function (e) stop("Could not convert grouping variable into a factor. Perhaps you entered the grouping variable first (instead of second)?")
  )

  data <- data %>%
    dplyr::mutate("{var2}" := base::as.factor({{ var2 }}))

  fctr_lvls <- base::levels(data[[str_of_var2]])

  if (base::length(fctr_lvls) != 2) {

    stop("The grouping variable must have two (and only two) levels. \n Perhaps you entered the grouping variable first (instead of second)?")

  }

  check_test(mosaic::t_test(formula, data = data, conf.level = conf_lvl))

  # code
  ind_test <- mosaic::t_test(formula, data = data, conf.level = conf_lvl)

  cl <- base::paste0(conf_lvl*100, "%")

  if (base::is.null(caption)) {

    caption <- base::paste("Two Sample Independent Means Test Between", str_of_var1, "and", str_of_var2,
                           "\n Confidence Level:", cl)

  } else {

    caption <- base::paste(caption, "\n Confidence Level:", cl)

  }

  n_na <- find_na(data, formula, n = 2)

  n1 <- base::nrow(data) - n_na[[1]]
  n2 <- base::nrow(data) - n_na[[2]]

  tibble::tibble(
    var = c(str_of_var1, str_of_var2),
    n = c(n1, n2),
    na = c(n_na[[1]], n_na[[2]]),
    # s = c("TBD", "TBD"),
    xbar = c(ind_test$estimate[[1]], ind_test$estimate[[2]]),
    se = c(ind_test$stderr, NA),
    t = c(ind_test$statistic, NA),
    df = c(ind_test$parameter, NA),
    p = c(base::format.pval(ind_test$p.value, digits = digits), NA)
  ) %>%
    finalize_tbl(digits = digits, caption = caption, na_str = "") %>%
    flextable::set_header_labels(var = "Variable", na = "n Missing", s = "Group s",
                                 xbar = "Group Means",
                                 se = "Standard Error",
                                 p = "p-value") %>%
    flextable::vline(j = 4)

  #   kbl(digits = 3,
  #       col.names = c("x-bar\u2081", "x-bar\u2082", "n\u2081", "n\u2082", "sd\u2081", "sd\u2082", "x-bar Difference", "Standard Error", "t", "df", "p-value")) %>%
  #   kable_styling(c('condensed', 'bordered', 'striped'), full_width = F) %>%
  #   row_spec(0, extra_css = "border-bottom: 3px solid black")

}


infer_reg <- function(data, formula, digits = 3, caption = NULL) {

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

  broom::tidy(model) %>%
    dplyr::select(-statistic, -p.value) %>%
    finalize_tbl(digits = digits, caption = caption) %>%
    flextable::set_header_labels(term = "Term", estimate = "Estimate", std.error = "Standard Error")

    # kbl(digits = 3,
    #     caption = paste0("Linear Model Coefficients Table: <br> F-test of ",
    #                      round(a_glance$statistic, 3), " on ", a_glance$df, " and ",
    #                      a_glance$df.residual, " degrees of freedom with an overall p-value of ",
    #                      overall_p, ".<br>", "R-Squared: ", round(a_glance$r.squared, 3), ".")) %>%
    # kable_styling(c('condensed', 'bordered', 'striped'), full_width = F) %>%
    # row_spec(0, extra_css = "border-bottom: 3px solid black")

}


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




