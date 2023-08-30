#' Create numerical summaries
#'
#' `tbl_num_sum()` is a wrapper around `mosaic::favstats()` and creates a tidy table of summary statistics
#'    including the min, q1, median, q3, max, mean, standard deviation, n, and missing values.
#'
#' @param data A data frame (or tibble).
#' @param formula Variables to summarize given in formula notation: `~var1` or `var1~var2`.
#'    `var1` should be numeric and `var2`, when supplied, should be a grouping variable.
#' @param digits The number if digits to round to. Defaults to 3.
#' @param na_rm Should missing values be removed? Defaults to FALSE.
#' @param caption An override for the table caption. A sensible default is given.
#'
#' @return An object of class flextable. If in an interactive session, the table will be
#'    viewable immediately.
#' @export
#'
#' @examples
#' tbl_num_sum(mtcars, ~wt)
#'
#' tbl_num_sum(mtcars, ~wt, na_rm = TRUE)
#' tbl_num_sum(mtcars, ~wt, na_rm = TRUE, digits = 2, caption = "This is a table")
#' tbl_num_sum(mtcars, wt~cyl, na_rm = TRUE)
#'
#' # not removing NAs is not recommended
#' tbl_num_sum(airquality, ~Ozone)
#' tbl_num_sum(airquality, Ozone~Month)
#'
#' # easy fix
#' tbl_num_sum(airquality, Ozone~Month, na_rm = TRUE)
tbl_num_sum <- function(data, formula, digits = 3, na_rm = FALSE, caption = NULL) {

  check_test(mosaic::favstats(x = formula, data = data, na.rm = na_rm))

  # code
  if (base::length(formula) == 2) {

    n_na <- find_na(data, formula)

    if (n_na == 0 & na_rm == FALSE) {

      na_rm <- TRUE

    } else if (n_na != 0 & na_rm == FALSE) {

      warning("NAs were detected but not removed. You may get missing values in your output.")

    }

    ind_var <- formula[[2]]
    ind_str <- base::deparse(base::substitute(ind_var))

    if (is.null(caption)) {
      caption <- base::paste("Summary Statistics for", ind_str)
    }

    mosaic::favstats(x = formula, data = data, na.rm = na_rm) %>%
      tibble::as_tibble() %>%
      finalize_tbl(digits, striped = FALSE, caption = caption)

  }
  else if (base::length(formula) == 3) {
    ind_var <- formula[[2]]
    ind_str <- base::deparse(base::substitute(ind_var))

    dep_var <- formula[[3]]
    dep_str <- base::deparse(base::substitute(dep_var))

    if (base::is.null(caption)) {
      caption <- base::paste("Summary Statistics By Group:", ind_str, "by", dep_str)
    }

    n_na <- find_na(data, formula, n = 2)

    if (n_na[[1]] == 0 & n_na[[2]] == 0 & na_rm == FALSE) {

      na_rm <- TRUE

    } else if ((n_na[[1]] != 0 | n_na[[2]] != 0) & na_rm == FALSE) {

      warning("NAs were detected but not removed. You may get missing values in your output.")

    }

    dep_lvls <- data %>%
      dplyr::select({{ dep_var }}) %>%
      stats::na.omit() %>%
      base::unique() %>%
      base::nrow()

    mosaic::favstats(x = formula, data = data, na.rm = na_rm) %>%
      tibble::as_tibble() %>%
      # dplyr::mutate(missing = c(n_na, base::rep("", times = n_lvls - 1))) %>%
      # dplyr::select(-missing) %>%
      finalize_tbl(digits,
                   caption = base::paste(caption, "\n", ind_str, "Missing:", n_na[[1]], "|", dep_str, "Missing:", n_na[[2]]))
  }

}

#' Create percentile summaries
#'
#' `tbl_pctile()` is a wrapper around `mosaic::quantile()` and creates a tidy table of data values
#'    at the given percentiles.
#'
#' @inheritParams tbl_num_sum
#' @param probs A vector of percentiles to compute. Each value must be between 0 and 1, inclusive.
#'    Defaults to 0, 25%, 50%, 75%, and 100%.
#'
#' @return An object of class flextable. If in an interactive session, the table will be
#'    viewable immediately.
#' @export
#'
#' @examples
#' tbl_pctile(mtcars, ~wt)
#' tbl_pctile(mtcars, ~wt, probs = c(.17, .3, .5, .7, .9, 1), na_rm = TRUE)
#' tbl_pctile(mtcars, wt~cyl, na_rm = TRUE)
#'
#' try(tbl_pctile(mtcars, ~wt, probs = c(25, 50, 75, 100)))
tbl_pctile <- function(data, formula, digits = 3, probs = c(0, .25, .5, .75, 1), caption = NULL, na_rm = FALSE) {

  # error catching

  valid_probs <- dplyr::between(probs, 0, 1)

  if (FALSE %in% valid_probs) {
    base::stop("You seem to have entered an invalid entry to the probs argument. These values should be between 0 and 1 (inclusive).")
  }

  check_test(mosaic::quantile(x = formula, data = data, na.rm = na_rm, prob = probs))

  # code
  if (base::length(formula) == 2) {

    var <- formula[[2]]
    var_str <- base::deparse(base::substitute(var))

    if (base::is.null(caption)) {
      caption <- base::paste("Percentiles on Variable", var_str)
    }

    na <- find_na(data, formula)

    mosaic::quantile(x = formula, data = data, na.rm = na_rm, prob = probs) %>%
      tibble::enframe() %>%
      tidyr::pivot_wider(names_from = name, values_from = value) %>%
      finalize_tbl(digits,
                   caption = base::paste(caption, "\n", "Missing:", na),
                   striped = FALSE) %>%
      flextable::set_header_labels(name = "Percentile", value = "Value")

  } else if (base::length(formula) > 2) {


    ind_var <- formula[[2]]
    ind_str <- base::deparse(base::substitute(ind_var))

    dep_var <- formula[[3]]
    dep_str <- base::deparse(base::substitute(dep_var))

    if (base::is.null(caption)) {
      caption <- base::paste("Percentiles on Variable", ind_str, "by", dep_str)
    }

    na <- find_na(data, formula, n = 2)

    mosaic::quantile(x = formula, data = data, na.rm = na_rm, prob = probs) %>%
      finalize_tbl(digits,
                   caption = base::paste(caption, "\n", ind_str, "Missing:", na[[1]], "|", dep_str, "Missing:", na[[2]])) %>%
      flextable::set_header_labels(name = "Percentile", value = "Value")

  }

}


#' Create a simple correlation table
#'
#' @inheritParams tbl_num_sum
#' @param formula Two variables given in formula notation: `var1~var2`.
#'
#' @return An object of class flextable. If in an interactive session, the table will be
#'    viewable immediately.
#' @export
#'
#' @examples
#' # Not removing NAs is not recommended
#' tbl_corr(airquality, Ozone~Solar.R)
#'
#' tbl_corr(airquality, Ozone~Solar.R, na_rm = TRUE)
tbl_corr <- function(data, formula, digits = 3, caption = NULL, na_rm = FALSE) {

  # error catching
  if (na_rm == FALSE) {
    warning("Missing values *not* automatically removed from calculation. \n You may get NA values in your output.")
  }

  check_test(mosaic::cor(formula, data = data))


  # code
  ind_var <- formula[[2]]
  ind_str <- base::deparse(base::substitute(ind_var))

  dep_var <- formula[[3]]
  dep_str <- base::deparse(base::substitute(dep_var))

  n_ind <- data %>%
    dplyr::select({{ ind_var }}) %>%
    base::nrow()

  n_dep <- data %>%
    dplyr::select({{ dep_var }}) %>%
    base::nrow()

  na <- find_na(data, formula, n = 2)

  if (na_rm == TRUE) {

    obs_used <- base::nrow(data %>% dplyr::select({{ ind_var }}, {{ dep_var }}) %>% stats::na.omit())

  } else if (na_rm == FALSE) {

    obs_used <- base::nrow(data %>% dplyr::select({{ ind_var }}, {{ dep_var }}))

  }

  if (base::is.null(caption)) {
    caption <- paste("Correlation of", ind_str, "vs.", dep_str)
  }

  tibble::tibble(n_ind = n_ind,
         na_ind = na[[1]],
         n_dep = n_dep,
         na_dep = na[[2]],
         obs_used = obs_used,
         corr = mosaic::cor(formula, data = data, use = base::ifelse(na_rm == FALSE, "everything", "complete"))) %>%
    finalize_tbl(digits = 3,
                 caption = caption,
                 striped = FALSE) %>%
    flextable::set_header_labels(n_ind = "n\u2081",
                                 na_ind = "n\u2081 missing",
                                 n_dep = "n\u2082",
                                 na_dep = "n\u2082 missing",
                                 obs_used = "Observations Used",
                                 corr = "Correlation")

}
