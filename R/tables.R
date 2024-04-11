#' Create a one way data table
#'
#' `tbl_1var()` creates a table giving counts and percentages on one variable.
#'
#' @param data A data frame.
#' @param formula The variable to tabulate. Should be given in formula notation, `~var`.
#' @param digits The number of digits to round to. Defaults to 3.
#' @param caption An optional caption for the table. Defaults to `NULL`, but a sensible default is
#'    provided within the function.
#' @param with_prop Should proportions be supplied with instead of percents? Defaults to "no",
#'    could also be "yes".
#' @param na_rm Should missing values be removed? Defaults to FALSE.
#'
#' @returns An object of class flextable. If in an interactive session, the table will be
#'    viewable immediately.
#' @export
#'
#' @examples
#' tbl_1var(mtcars, ~gear)
#' tbl_1var(mtcars, ~gear, digits = 2)
#'
#' # Will give an error
#' try(tbl_1var(mtcars, ~Gear))
tbl_1var <- function(data, formula, digits = 3, caption = NULL, with_prop = c("no", "yes"), na_rm = FALSE) {

  # check for empty strings and make them actual NAs
  data <- tibble::as_tibble(data) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., "")))

  # error catching
  check_test(mosaic::tally(formula, data = data, format = "count"))
  with_prop <- base::match.arg(with_prop)

  na <- find_na(data, formula)

  if (na_rm == TRUE) data <- data %>% stats::na.omit()

  # code
  var <- formula[[2]]
  var_str <- base::deparse(base::substitute(var))

  if (base::is.null(caption)) {

    caption <- base::paste("One Way Counts on Variable", var_str, "\n", "Missing:", na)

  } else {

    caption <- base::paste(caption, "\n", "Missing:", na)

  }

  if (with_prop == "no") {

    output <- mosaic::tally(formula, data = data, format = "count") %>%
      tibble::as_tibble() %>%
      dplyr::mutate(pct = (n / base::nrow(data))*100) %>%
      janitor::adorn_totals("row") %>%
      finalize_tbl(digits = digits, caption = base::paste(caption, "\n NAs Removed: ", base::ifelse(na_rm == FALSE, "No", "Yes"))) %>%
      flextable::set_header_labels(n = "Count", pct = "Percent") %>%
      flextable::fontsize(size = 9, part = "all")

  } else if (with_prop == "yes") {

    output <- mosaic::tally(formula, data = data, format = "count") %>%
      tibble::as_tibble() %>%
      dplyr::mutate(pct = (n / base::nrow(data))) %>%
      janitor::adorn_totals("row") %>%
      finalize_tbl(digits = digits, caption = base::paste(caption, "\n NAs Removed: ", base::ifelse(na_rm == FALSE, "No", "Yes"))) %>%
      flextable::set_header_labels(n = "Count", pct = "Proportion") %>%
      flextable::fontsize(size = 9, part = "all")

  }

  output

}


#' Create a two way data table
#'
#' @inheritParams tbl_1var
#' @param formula The variables to tabulate. Should be given in formula notation `var1~var2`. Changing
#'    the order of the variables will swap the table axes.
#' @param row_pct Should row percents be included in each cell? Defaults to "hide" with the only
#'    other possible value being "show".
#'
#' @return An object of class flextable. If in an interactive session, the table will be
#'    viewable immediately.
#' @export
#'
#' @examples
#' tbl_2var(mtcars, cyl~gear)
#' tbl_2var(mtcars, cyl~gear, row_pct = "show")
#' tbl_2var(mtcars, cyl~gear, caption = "This is the new caption")
#'
#' # Will give an error
#' try(tbl_2var(mtcars, Cyl~Gear))
tbl_2var <- function(data, formula, row_pct = c("hide", "show"), digits = 3, caption = NULL, na_rm = FALSE) {

  # check for empty strings and make them actual NAs
  data <- tibble::as_tibble(data) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., "")))

  # error catching
  check_test(mosaic::tally(formula, data = data))

  row_pct <- base::match.arg(row_pct)

  na <- find_na(data, formula, n = 2)
  if (na_rm == TRUE) data <- data %>% stats::na.omit()

  # code
  var1 <- formula[[2]]
  var2 <- formula[[3]]
  var1_str <- base::deparse(base::substitute(var1))
  var2_str <- base::deparse(base::substitute(var2))

  if (base::is.numeric(data[, var1_str])) {

    var1_lvls <- base::length(base::unique(data[, var1_str]))

  } else {

    var1_lvls <- base::nrow(base::unique(data[, var1_str]))

  }

  # table with no row percents
  if (row_pct == "hide") {

    if (base::is.null(caption)) {
      caption <- base::paste("Two-Way Counts of", var1, "vs.", var2, "\n", var1_str, "Missing:", na[[1]],
                             "|", var2_str, "Missing:", na[[2]])
    } else {
      caption <- base::paste(caption, "\n", var1_str, "Missing:", na[[1]],
                             "|", var2_str, "Missing:", na[[2]])
    }

    starter <- mosaic::tally(formula, data = data) %>%
      tibble::as_tibble() %>%
      tidyr::pivot_wider(names_from = {{ var1 }}, values_from = n) %>%
      janitor::adorn_totals(c("col", "row")) %>%
      dplyr::mutate(Total = base::as.integer(Total))

    starter %>%
      finalize_tbl(digits = digits, caption = base::paste(caption, "\n NAs Removed:", base::ifelse(na_rm == FALSE, "No", "Yes"))) %>%
      flextable::add_header_row(values = c("", var1_str, ""),
                                colwidths = c(1, var1_lvls, 1)) %>%
      flextable::vline(j = c(1, var1_lvls + 1),
                       border = officer::fp_border(width = 1.5)) %>%
      flextable::hline(i = nrow(starter) - 1,
                       border = officer::fp_border(width = 1.5)) %>%
      flextable::hline(i = 1, part = "header") %>%
      flextable::hline(i = 1, j = c(1, var1_lvls + 2), part = "header",
                       border = officer::fp_border(color = NA)) %>%
      flextable::bold(j = 1) %>%
      flextable::fontsize(size = 9, part = "all")

  }

  # table with row percentages
  else if (row_pct == "show") {

    if (base::is.null(caption)) {
      caption <- base::paste("Two-Way Counts (with Row Percentages) of", var1, "vs.", var2, "\n", var1_str, "Missing:", na[[1]],
                             "|", var2_str, "Missing:", na[[2]])
    } else {
      caption <- base::paste(caption, "\n", var1_str, "Missing:", na[[1]],
                             "|", var2_str, "Missing:", na[[2]])
    }

    starter <- mosaic::tally(formula, data = data) %>%
      tibble::as_tibble() %>%
      tidyr::pivot_wider(names_from = {{ var1 }}, values_from = n) %>%
      janitor::adorn_totals(c("col", "row")) %>%
      janitor::adorn_percentages("row") %>%
      janitor::adorn_pct_formatting(digits = 2) %>%
      janitor::adorn_ns(position = "front") %>%
      dplyr::mutate(dplyr::across(-1, ~sub(" ", "\n", .)))

    starter %>%
      finalize_tbl(digits = digits, caption = base::paste(caption, "\n NAs Removed:", base::ifelse(na_rm == FALSE, "No", "Yes"))) %>%
      flextable::add_header_row(values = c("", var1_str, ""),
                                colwidths = c(1, var1_lvls, 1)) %>%
      flextable::vline(j = c(1, var1_lvls + 1),
                       border = officer::fp_border(width = 1.5)) %>%
      flextable::hline(i = nrow(starter) - 1,
                       border = officer::fp_border(width = 1.5)) %>%
      flextable::hline(i = 1, part = "header") %>%
      flextable::hline(i = 1, j = c(1, var1_lvls + 2), part = "header",
                       border = officer::fp_border(color = NA)) %>%
      flextable::bold(j = 1) %>%
      flextable::fontsize(size = 9, part = "all")


  }
}
