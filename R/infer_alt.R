#' All-in-one alternate two-sample proportion test
#'
#' `infer_2prop()` is an alternative to [infer_2prop_test()] and [infer_2prop_int()]. Rather than have
#'    hypothesis test and confidence interval output split into two separate functions, you can now do it
#'    in one. For just a hypothesis test, do nothing different from [infer_2prop_test()] (except change
#'    the function name). For a confidence interval provided with that, use `conf_int = "show"`.
#'
#' @inheritParams infer_2prop_test
#' @param conf_int Should a confidence interval be provided in addition to the hypothesis test output?
#'   Defaults to "hide" with the other option being "show".
#'
#' @return An object of class flextable. In interactive sessions, output is viewable immediately.
#' @export
#'
#' @examples
#' infer_2prop(mtcars, vs~am, success = 1)
#' infer_2prop(mtcars, vs~am, success = 1, conf_lvl = .9, digits = 4)
#' infer_2prop(mtcars, vs~am, success = 1, conf_lvl = .9, digits = 5, conf_int = "show")
infer_2prop <- function(data, formula, success, digits = 3, conf_lvl = 0.95, conf_int = c("hide", "show"), caption = NULL) {

  # check for empty strings and make them actual NAs
  data <- tibble::as_tibble(data) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., "")))

  # error catching
  check_conf_lvl(conf_lvl)

  check_test(mosaic::prop.test(formula, data = data, conf.level = conf_lvl, success = success, correct = FALSE))

  conf_int = base::match.arg(conf_int)

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
                           "\nSuccess:", success, "| Confidence:", cl)

  } else {

    caption <- base::paste(caption, "\nSuccess:", success, "| Confidence:", cl)

  }

  grp_lvls <- base::sort(base::unique(dplyr::pull(data, grp_var)))

  # find NAs
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

  # find n successes
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
  z_star <- round(stats::qnorm((1 - conf_lvl) / 2, lower.tail = FALSE), 3)
  moe <- se*z_star

  # table without CI
  no_interval <- tibble::tibble(
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
    flextable::set_header_labels(var = grp_str, yay = "n\nSuccesses", na = "n\nMissing", phat = "p\u0302",
                                 se = "Standard\nError", p = "p-value\n(2 tail)") %>%
    flextable::vline(j = 5) %>%
    fit_tbl()

  # table with CI
  # tk check z and cil and ciu (and point estimate)
  interval <- tibble::tibble(
    var = base::as.character(grp_lvls),
    yay = c(yay1$n, yay2$n),
    n = c(n1, n2),
    na = c(na1, na2),
    phat = c(two_prop$estimate[[1]], two_prop$estimate[[2]]),
    se = c(se, NA),
    z = c(two_prop$statistic, NA),
    p = c(base::ifelse(two_prop$p.value < 0.0001,
                       "< 0.0001",
                       base::format.pval(two_prop$p.value / 2, digits = digits)), NA),
    cil = c(two_prop$conf.int[[1]], NA),
    ciu = c(two_prop$conf.int[[2]], NA)
  ) %>%
    finalize_tbl(digits = digits,
                 caption = caption,
                 na_str = "") %>%
    flextable::set_header_labels(var = grp_str, yay = "n\nSuccesses", na = "n\nMissing", phat = "p\u0302",
                                 se = "Standard\nError", p = "p-value\n(1 tail)",
                                 cil = base::paste(cl, "\nInterval\nLower"),
                                 ciu = base::paste(cl, "\nInterval\nUpper")) %>%
    flextable::vline(j = c(5, 8)) %>%
    fit_tbl()

  if (conf_int == "hide") return(no_interval)
  else return(interval)

}

#' All-in-one alternate two-sample means test
#'
#' `infer_2mean()` is an alternative to [infer_2mean_test()] and [infer_2mean_int()]. Rather than have
#'    hypothesis test and confidence interval output split into two separate functions, you can now do it
#'    in one. For just a hypothesis test, do nothing different from [infer_2mean_test()] (except change
#'    the function name). For a confidence interval provided with that, use `conf_int = "show`.
#'
#' @inheritParams infer_2mean_test
#' @param conf_int Should a confidence interval be provided in addition to the hypothesis test output?
#'   Defaults to "hide" with the other option being "show".
#'
#' @return An object of class flextable. In interactive sessions, output is viewable immediately.
#' @export
#'
#' @examples
#' infer_2mean(mtcars, wt~vs)
#' infer_2mean(mtcars, wt~vs, conf_lvl = .9)
#' infer_2mean(mtcars, wt~vs, conf_lvl = .9, conf_int = "show")
infer_2mean <- function(data, formula, digits = 3, conf_lvl = 0.95, conf_int = c("hide", "show"), caption = NULL) {

  # check for empty strings and make them actual NAs
  data <- tibble::as_tibble(data) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., "")))

  # error catching
  check_conf_lvl(conf_lvl)

  conf_int <- base::match.arg(conf_int)

  var1 <- formula[[2]]
  var1_str <- base::deparse(base::substitute(var1))

  grp_var <- formula[[3]]
  grp_str <- base::deparse(base::substitute(grp_var))

  # error catching
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

  check_test(mosaic::t_test(formula, data = data, conf.level = conf_lvl))

  # code
  ind_test <- mosaic::t_test(formula, data = data, conf.level = conf_lvl)

  cl <- base::paste0(conf_lvl*100, "%")

  # build caption
  if (base::is.null(caption)) {

    caption <- base::paste("Two Sample Independent Means Test Between", var1_str, "and", grp_str,
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

  # table without CI
  no_interval <- tibble::tibble(
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
    flextable::set_header_labels(var = grp_str, na = "n\nMissing", s = "Group s",
                                 xbar = "Group\nMeans",
                                 se = "Standard\nError",
                                 p = "p-value") %>%
    flextable::vline(j = 4) %>%
    fit_tbl()

  # table with CI
  interval <- tibble::tibble(
    var = base::as.character(grp_lvls),
    n = c(n1, n2),
    na = c(na1, na2),
    xbar = c(ind_test$estimate[[1]], ind_test$estimate[[2]]),
    se = c(ind_test$stderr, NA),
    t = c(ind_test$statistic, NA),
    df = c(ind_test$parameter, NA),
    p = c(base::ifelse(ind_test$p.value < 0.0001,
                       "< 0.0001",
                       base::format.pval(ind_test$p.value, digits = digits)), NA),
    cil = c(ind_test$conf.int[[1]], NA),
    ciu = c(ind_test$conf.int[[2]], NA)
  ) %>%
    finalize_tbl(digits = digits, caption = caption, na_str = "") %>%
    flextable::set_header_labels(var = grp_str, na = "n\nMissing", s = "Group s",
                                 xbar = "Group\nMeans",
                                 se = "Standard\nError",
                                 p = "p-value (2 tail)",
                                 cil = base::paste(cl, "\nInterval\nLower"),
                                 ciu = base::paste(cl, "\nInterval\nUpper")) %>%
    flextable::vline(j = c(4, 8)) %>%
    fit_tbl()

  if (conf_int == "hide") return(no_interval)
  else return(interval)

}
