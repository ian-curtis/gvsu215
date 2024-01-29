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
tbl_num_sum <- function(data, formula, digits = 3, caption = NULL, na_rm = FALSE) {

  # check for empty strings and make them actual NAs
  data <- tibble::as_tibble(data) %>%
    dplyr::mutate(dplyr::across(where(is.character), ~dplyr::na_if(., "")))

  check_test(mosaic::favstats(x = formula, data = data, na.rm = na_rm))

  og_na <- na_rm

  # code
  if (base::length(formula) == 2) { # non-grouped table

    n_na <- find_na(data, formula)

    # need this to alert if NAs were present but not removed

    if (n_na == 0 & na_rm == FALSE) {

      na_rm <- TRUE

    } else if (n_na != 0 & na_rm == FALSE) {

      cli::cli_alert_warning("NAs were detected but not removed. You may get missing values in your output.")

    }

    var1 <- formula[[2]]
    var1_str <- base::deparse(base::substitute(var1))

    if (is.null(caption)) {
      caption <- base::paste("Summary Statistics for", var1_str)
    }

    df <- mosaic::favstats(x = formula, data = data, na.rm = na_rm) %>%
      tibble::as_tibble()

    if (og_na == TRUE) {

      df %>%
        dplyr::select(-missing) %>%
        finalize_tbl(digits, striped = FALSE,
                     caption = base::paste(caption, "\n NAs Removed:",
                                           ifelse(og_na == TRUE, "Yes", "No"))) %>%
        flextable::fontsize(size = 9, part = "all")

    } else if (og_na == FALSE) {

      df %>%
        finalize_tbl(digits, striped = FALSE,
                     caption = base::paste(caption, "\n NAs Removed:",
                                                    ifelse(og_na == TRUE, "Yes", "No"))) %>%
        flextable::fontsize(size = 9, part = "all")

    }


  }
  else if (base::length(formula) == 3) { # grouped table

    var1 <- formula[[2]]
    var1_str <- base::deparse(base::substitute(var1))

    var2 <- formula[[3]]
    var2_str <- base::deparse(base::substitute(var2))

    if (base::is.null(caption)) {
      caption <- base::paste("Summary Statistics By Group:", var1_str, "by", var2_str)
    }

    n_na <- find_na(data, formula, n = 2)

    # need this to alert if NAs were present but not removed
    if (n_na[[1]] == 0 & n_na[[2]] == 0 & na_rm == FALSE) {

      na_rm <- TRUE

    } else if ((n_na[[1]] != 0 | n_na[[2]] != 0) & na_rm == FALSE) {

      cli::cli_alert_warning("NAs were detected but not removed. You may get missing values in your output.")

    }

    # find number of levels of dependent variable
    dep_lvls <- data %>%
      dplyr::select({{ var2 }}) %>%
      stats::na.omit() %>%
      base::unique() %>%
      base::nrow()

    df <- mosaic::favstats(x = formula, data = data, na.rm = na_rm) %>%
      tibble::as_tibble()



    if (og_na == TRUE) {

      df %>%
        dplyr::select(-missing) %>%
        finalize_tbl(digits,
                     caption = base::paste(caption, "\n NAs Removed:", base::ifelse(og_na == TRUE, "Yes", "No"))) %>%
        flextable::fontsize(size = 9, part = "all")

    } else if (og_na == FALSE) {

      df %>%
        finalize_tbl(digits,
                     caption = base::paste(caption, "\n NAs Removed:", base::ifelse(og_na == TRUE, "Yes", "No"))) %>%
        flextable::fontsize(size = 9, part = "all")

    }

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
#' tbl_pctile(mtcars, ~wt, probs = c(.17, .3, .5, .7, .9, 1))
#' tbl_pctile(mtcars, wt~cyl, digits = 4)
#' tbl_pctile(airquality, ~Solar.R)
#' tbl_pctile(airquality, Month~Solar.R)
#'
#' try(tbl_pctile(mtcars, ~wt, probs = c(25, 50, 75, 100)))
tbl_pctile <- function(data, formula, digits = 3, probs = c(0, .25, .5, .75, 1), caption = NULL) {

  # check for empty strings and make them actual NAs
  data <- tibble::as_tibble(data) %>%
    dplyr::mutate(dplyr::across(where(is.character), ~dplyr::na_if(., "")))

  # error catching

  valid_probs <- dplyr::between(probs, 0, 1)

  if (FALSE %in% valid_probs) {
    cli::cli_abort("You seem to have entered an invalid entry to the {.var probs} argument. These values should be between 0 and 1 (inclusive).")
  }


  check_test(mosaic::quantile(x = formula, data = data, na.rm = TRUE, prob = probs))

  # code
  if (base::length(formula) == 2) {

    var <- formula[[2]]
    var_str <- base::deparse(base::substitute(var))

    na <- find_na(data, formula)

    if (na > 1) {

      rlang::inform("Note: NAs always removed for percentile tables", .frequency = "once", .frequency_id = "pctile-nas")

    }

    if (base::is.null(caption) & na > 1) {

      caption <- base::paste("Percentiles For", var_str, "\n", "Missing:", na, "| NAs Removed: Yes")

    } else if (base::is.null(caption)) {

      caption <- base::paste("Percentiles For", var_str, "\n", "Missing:", na)

    }


    mosaic::quantile(x = formula, data = data, na.rm = TRUE, prob = probs) %>%
      tibble::enframe() %>%
      tidyr::pivot_wider(names_from = name, values_from = value) %>%
      finalize_tbl(digits,
                   caption = caption,
                   striped = FALSE) %>%
      flextable::set_header_labels(name = "Percentile", value = "Value") %>%
      flextable::fontsize(size = 9, part = "all")

  } else if (base::length(formula) > 2) { # grouped table


    var1 <- formula[[2]]
    var1_str <- base::deparse(base::substitute(var1))

    var2 <- formula[[3]]
    var2_str <- base::deparse(base::substitute(var2))

    na <- find_na(data, formula, n = 2)

    if (na[[1]] > 0 | na[[2]] > 0) {

      rlang::inform("Note: NAs always removed for percentile tables", .frequency = "once", .frequency_id = "pctile-nas")

    }


    if (na[[1]] == 0 & na[[2]] == 0 & base::is.null(caption)) {

      caption <- base::paste("Percentiles For", var1_str, "by", var2_str,  "\n", var1_str, "Missing:", na[[1]], "|",
                             var2_str, "Missing:", na[[2]])

    } else if (base::is.null(caption)) {

      caption <- base::paste("Percentiles For", var1_str, "by", var2_str,  "\n", var1_str, "Missing:", na[[1]], "|",
                             var2_str, "Missing:", na[[2]], "| NAs Removed: Yes")

    }

    mosaic::quantile(x = formula, data = data, na.rm = TRUE, prob = probs) %>%
      finalize_tbl(digits,
                   caption = caption) %>%
      flextable::set_header_labels(name = "Percentile", value = "Value") %>%
      flextable::fontsize(size = 9, part = "all")

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

  # check for empty strings and make them actual NAs
  data <- tibble::as_tibble(data) %>%
    dplyr::mutate(dplyr::across(where(is.character), ~dplyr::na_if(., "")))

  # error catching
  if (na_rm == FALSE) {
    rlang::inform("Missing values *not* automatically removed from calculation. \n You may get NA values in your output.", .frequency = "once", .frequency_id = "corr-nas")
  }

  check_test(mosaic::cor(formula, data = data))


  # code
  var1 <- formula[[2]]
  var1_str <- base::deparse(base::substitute(var1))

  var2 <- formula[[3]]
  var2_str <- base::deparse(base::substitute(var2))

  n_ind <- data %>%
    dplyr::select({{ var1 }}) %>%
    base::nrow()

  n_dep <- data %>%
    dplyr::select({{ var2 }}) %>%
    base::nrow()

  na <- find_na(data, formula, n = 2)

  # find the number of observations used (total number of rows after removing all NAs)
  if (na_rm == TRUE) {

    obs_used <- base::nrow(data %>% dplyr::select({{ var1 }}, {{ var2 }}) %>% stats::na.omit())

  } else if (na_rm == FALSE) {

    obs_used <- base::nrow(data %>% dplyr::select({{ var1 }}, {{ var2 }}))

  }

  if (base::is.null(caption)) {
    caption <- paste("Correlation of", var1_str, "vs.", var2_str)
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
    flextable::set_header_labels(n_ind = base::paste(var1_str, "n"),
                                 na_ind = base::paste(var1_str, "missing"),
                                 n_dep = base::paste(var2_str, "n"),
                                 na_dep = base::paste(var2_str, "missing"),
                                 obs_used = "Observations Used",
                                 corr = "Correlation") %>%
    flextable::fontsize(size = 9, part = "all")

}
