num_sum <- function(data, formula, digits = 3, na_rm = FALSE, caption = NULL, ...) {

  # error catching
  if (na_rm == FALSE) {
    base::warning("Missing values *not* automatically removed from calculation. \n You may get NA values in your output.")
  }

  check_test(mosaic::favstats(x = formula, data = data, na.rm = na_rm, ...))

  # code
  if (base::length(formula) == 2) {
    ind_var <- formula[[2]]
    ind_str <- base::deparse(base::substitute(ind_var))

    if (is.null(caption)) {
      caption <- base::paste("Summary Statistics for", ind_str)
    }

    mosaic::favstats(x = formula, data = data, na.rm = na_rm, ...) %>%
      tibble::as_tibble() %>%
      finalize_tbl(digits, striped = FALSE, caption = caption)
    # kableExtra::kbl(digits = digits, caption = caption) %>%
    # kableExtra::kable_styling(c('condensed', 'bordered', 'striped'), full_width = F) %>%
    # kableExtra::row_spec(0, extra_css = "border-bottom: 3px solid black")
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

    dep_lvls <- data %>%
      dplyr::select({{ dep_var }}) %>%
      stats::na.omit() %>%
      base::unique() %>%
      base::nrow()

    mosaic::favstats(x = formula, data = data, na.rm = na_rm, ...) %>%
      tibble::as_tibble() %>%
      # dplyr::mutate(missing = c(n_na, base::rep("", times = n_lvls - 1))) %>%
      dplyr::select(-missing) %>%
      finalize_tbl(digits,
                   caption = base::paste(caption, "\n", ind_str, "Missing:", n_na[[1]], "|", dep_str, "Missing:", n_na[[2]]))
      # flextable::vline(j = 9, border = officer::fp_border())
  }

}

pctile <- function(data, formula, digits = 3, probs = c(0, .25, .5, .75, 1), caption = NULL, na_rm = FALSE) {

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
                   caption = base::paste(caption, "\n", "Missing:", na)) %>%
      flextable::set_header_labels(name = "Percentile", value = "Value")
    # kbl(col.names = c("Percentile", "Value")) %>%
    # kable_styling(c('condensed', 'bordered', 'striped'), full_width = F) %>%
    # row_spec(0, extra_css = "border-bottom: 3px solid black")

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


corr <- function(data, formula, digits = 3, caption = NULL, na_rm = FALSE) {

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

  if (base::is.null(caption)) {
    caption <- paste("Correlation of", ind_str, "vs.", dep_str)
  }

  tibble::tibble(n_ind = n_ind,
         na_ind = na[[1]],
         n_dep = n_dep,
         na_dep = na[[2]],
         corr = mosaic::cor(formula, data = data, use = base::ifelse(na_rm == FALSE, "everything", "complete"))) %>%
    finalize_tbl(digits = 3,
                 caption = caption) %>%
    flextable::set_header_labels(n_ind = "n₁",
                                 na_ind = "n₁ missing",
                                 n_dep = "n₂",
                                 na_dep = "n₂ missing",
                                 corr = "Correlation")
  # kbl(digits = 3, caption = "Correlation of MaxWeight vs. MaxHeight", col.names = c("n", "Correlation")) %>%
  # kable_styling(c('condensed', 'bordered'), full_width = F) %>%
  # row_spec(0, extra_css = "border-bottom: 3px solid black")

}
