#' All-in-one alternate two-sample proportion test
#'
#' `infer_2prop()` is an alternative to [infer_2prop_test()] and [infer_2prop_int()]. Rather than have
#'    hypothesis test and confidence interval output split into two separate functions, you can now do it
#'    in one. For just a hypothesis test, do nothing different from [infer_2prop_test()] (except change
#'    the function name). For a confidence interval provided with that, use `conf_int = TRUE`.
#'
#' @inheritParams infer_2prop_test
#' @param conf_int Should a confidence interval be provided in addition to the hypothesis test output?
#'   Defaults to FALSE.
#'
#' @return An object of class flextable. In interactive sessions, output is viewable immediately.
#' @export
#'
#' @examples
#' infer_2prop(mtcars, vs~am, success = 1)
#' infer_2prop(mtcars, vs~am, success = 1, conf_lvl = .9, digits = 4)
infer_2prop <- function(data, formula, success, digits = 3, conf_lvl = 0.95, conf_int = FALSE, caption = NULL) {

  # error catching
  check_conf_lvl(conf_lvl)

  check_test(mosaic::prop.test(formula, data = data, conf.level = conf_lvl, success = success, correct = FALSE))


  # code
  two_prop <- mosaic::prop.test(formula, data = data, conf.level = conf_lvl, success = success, correct = FALSE)

  var1 <- formula[[2]]
  str_of_var1 <- base::deparse(base::substitute(var1))

  grp_var <- formula[[3]]
  str_of_grp <- base::deparse(base::substitute(grp_var))

  cl <- base::paste0(conf_lvl*100, "%")

  if (base::is.null(caption)) {

    caption <- base::paste("Two Sample Proportion Test Between", str_of_var1, "and", str_of_grp,
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

  no_interval <- tibble::tibble(
    var = base::as.character(grp_lvls),
    yay = c(yay1$n, yay2$n),
    n = c(n1, n2),
    na = c(na1, na2),
    phat = c(two_prop$estimate[[1]], two_prop$estimate[[2]]),
    se = c(sqrt((two_prop$estimate[[1]]*(1-two_prop$estimate[[1]])/n1) + (two_prop$estimate[[2]]*(1-two_prop$estimate[[2]])/n2)), NA),
    z = c(two_prop$statistic, NA),
    p = c(base::format.pval(two_prop$p.value, digits = digits), NA)
  ) %>%
    finalize_tbl(digits = digits,
                 caption = caption,
                 na_str = "") %>%
    flextable::set_header_labels(var = str_of_grp, yay = "n Success", na = "n Missing", phat = "p-hat",
                                 se = "Standard Error", p = "p-value") %>%
    flextable::vline(j = 5)

  interval <- tibble::tibble(
    var = base::as.character(grp_lvls),
    yay = c(yay1$n, yay2$n),
    n = c(n1, n2),
    na = c(na1, na2),
    phat = c(two_prop$estimate[[1]], two_prop$estimate[[2]]),
    se = c(sqrt((two_prop$estimate[[1]]*(1-two_prop$estimate[[1]])/n1) + (two_prop$estimate[[2]]*(1-two_prop$estimate[[2]])/n2)), NA),
    z = c(two_prop$statistic, NA),
    p = c(base::format.pval(two_prop$p.value, digits = digits), NA),
    cil = c(two_prop$conf.int[[1]], NA),
    ciu = c(two_prop$conf.int[[2]], NA)
  ) %>%
    finalize_tbl(digits = digits,
                 caption = caption,
                 na_str = "") %>%
    flextable::set_header_labels(var = str_of_grp, yay = "n Success", na = "n Missing", phat = "p-hat",
                                 se = "Standard Error", p = "p-value",
                                 cil = base::paste(cl, "Interval Lower"),
                                 ciu = base::paste(cl, "Interval Upper")) %>%
    flextable::vline(j = c(5, 8))

  if (conf_int == FALSE) return(no_interval)
    else return(interval)

}

#' All-in-one alternate two-sample means test
#'
#' `infer_2mean()` is an alternative to [infer_2mean_test()] and [infer_2mean_int()]. Rather than have
#'    hypothesis test and confidence interval output split into two separate functions, you can now do it
#'    in one. For just a hypothesis test, do nothing different from [infer_2mean_test()] (except change
#'    the function name). For a confidence interval provided with that, use `conf_int = TRUE`.
#'
#' @inheritParams infer_2mean_test
#' @param conf_int Should a confidence interval be provided in addition to the hypothesis test output?
#'   Defaults to FALSE.
#'
#' @return An object of class flextable. In interactive sessions, output is viewable immediately.
#' @export
#'
#' @examples
#' infer_2mean_test(mtcars, wt~vs)
#' infer_2mean_test(mtcars, wt~vs, conf_lvl = .9)
infer_2mean <- function(data, formula, digits = 3, conf_lvl = 0.95, conf_int = FALSE, caption = NULL) {

  # error catching
  check_conf_lvl(conf_lvl)

  var1 <- formula[[2]]
  str_of_var1 <- base::deparse(base::substitute(var1))

  grp_var <- formula[[3]]
  str_of_grp <- base::deparse(base::substitute(grp_var))

  # error catching
  check_conf_lvl(conf_lvl)

  base::tryCatch(data %>% dplyr::mutate("{grp_var}" := base::factor({{ grp_var }})),
                 error = function (e) stop("Could not convert grouping variable into a factor. Perhaps you entered the grouping variable first (instead of second)?")
  )

  data <- data %>%
    dplyr::mutate("{grp_var}" := base::as.factor({{ grp_var }}))

  grp_lvls <- base::levels(data[[str_of_grp]])

  if (base::length(grp_lvls) != 2) {

    stop("The grouping variable must have two (and only two) levels. \n Perhaps you entered the grouping variable first (instead of second)?")

  }

  check_test(mosaic::t_test(formula, data = data, conf.level = conf_lvl))

  # code
  ind_test <- mosaic::t_test(formula, data = data, conf.level = conf_lvl)

  cl <- base::paste0(conf_lvl*100, "%")

  if (base::is.null(caption)) {

    caption <- base::paste("Two Sample Independent Means Test Between", str_of_var1, "and", str_of_grp,
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

  no_interval <- tibble::tibble(
    var = base::as.character(grp_lvls),
    n = c(n1, n2),
    na = c(na1, na2),
    xbar = c(ind_test$estimate[[1]], ind_test$estimate[[2]]),
    se = c(ind_test$stderr, NA),
    t = c(ind_test$statistic, NA),
    df = c(ind_test$parameter, NA),
    p = c(base::format.pval(ind_test$p.value, digits = digits), NA)
  ) %>%
    finalize_tbl(digits = digits, caption = caption, na_str = "") %>%
    flextable::set_header_labels(var = str_of_grp, na = "n Missing", s = "Group s",
                                 xbar = "Group Means",
                                 se = "Standard Error",
                                 p = "p-value") %>%
    flextable::vline(j = 4)

  interval <- tibble::tibble(
    var = base::as.character(grp_lvls),
    n = c(n1, n2),
    na = c(na1, na2),
    xbar = c(ind_test$estimate[[1]], ind_test$estimate[[2]]),
    se = c(ind_test$stderr, NA),
    t = c(ind_test$statistic, NA),
    df = c(ind_test$parameter, NA),
    p = c(base::format.pval(ind_test$p.value, digits = digits), NA),
    cil = c(ind_test$conf.int[[1]], NA),
    ciu = c(ind_test$conf.int[[2]], NA)
  ) %>%
    finalize_tbl(digits = digits, caption = caption, na_str = "") %>%
    flextable::set_header_labels(var = str_of_grp, na = "n Missing", s = "Group s",
                                 xbar = "Group Means",
                                 se = "Standard Error",
                                 p = "p-value",
                                 cil = base::paste(cl, "Interval Lower"),
                                 ciu = base::paste(cl, "Interval Upper")) %>%
    flextable::vline(j = c(4, 8))

  if (conf_int == FALSE) return(no_interval)
    else return(interval)

}