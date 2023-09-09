#' Create a one way data table
#'
#' `tbl_1var()` creates a table giving counts and percentages on one variable.
#'
#' @param data A data frame.
#' @param formula The variable to tabulate. Should be given in formula notation, `~var`.
#' @param digits The number of digits to round to. Defaults to 3.
#' @param caption An optional caption for the table. Defaults to `NULL`, but a sensible default is
#'    provided within the function.
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
tbl_1var <- function(data, formula, digits = 3, caption = NULL) {

  # error catching
  check_test(mosaic::tally(formula, data = data, format = "count"))


  # code
  var <- formula[[2]]
  var_str <- base::deparse(base::substitute(var))

  if (base::is.null(caption)) {
    caption <- base::paste("One Way Counts on Variable", var_str)
  }

  mosaic::tally(formula, data = data, format = "count") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(pct = (n / base::nrow(data))*100) %>%
    janitor::adorn_totals("row") %>%
    finalize_tbl(digits = digits, caption = base::paste("One Way Counts on Variable", var_str)) %>%
    flextable::set_header_labels(n = "Count", pct = "Percent")
    # flextable::set_caption(caption = flextable::as_paragraph(flextable::as_chunk(caption)))

}


#' Create a two way data table
#'
#' @inheritParams tbl_1var
#' @param formula The variables to tabulate. Should be given in formula notation `var1~var2`. Changing
#'    the order of the variables will swap the table axes.
#' @param row_pct Should row percents be included in each cell? Defaults to FALSE with the only
#'    other possible value being TRUE.
#'
#' @return An object of class flextable. If in an interactive session, the table will be
#'    viewable immediately.
#' @export
#'
#' @examples
#' tbl_2var(mtcars, cyl~gear)
#' tbl_2var(mtcars, cyl~gear, row_pct = TRUE)
#' tbl_2var(mtcars, cyl~gear, caption = "This is the new caption")
#'
#' # Will give an error
#' try(tbl_2var(mtcars, Cyl~Gear))
tbl_2var <- function(data, formula, row_pct = FALSE, digits = 3, caption = NULL) {

  # error catching
  check_test(mosaic::tally(formula, data = data))

  # code
  var1 <- formula[[2]]
  var2 <- formula[[3]]
  var1_str <- base::deparse(base::substitute(var1))
  var2_str <- base::deparse(base::substitute(var2))

  # data <- data %>%
  #   dplyr::mutate("{var1}" := base::as.factor({{ var1 }}),
  #                 "{var2}" := base::as.factor({{ var2 }}))

  if (base::is.numeric(data[, var1_str])) {
    var1_lvls <- base::length(base::unique(data[, var1_str]))
  } else {
    var1_lvls <- base::nrow(base::unique(data[, var1_str]))
  }

  if (row_pct == FALSE) {

    if (base::is.null(caption)) {
      caption <- base::paste("Two-Way Counts of", var1, "vs.", var2)
    }

    mosaic::tally(formula, data = data) %>%
      tibble::as_tibble() %>%
      tidyr::pivot_wider(names_from = {{ var1 }}, values_from = n) %>%
      janitor::adorn_totals("col") %>%
      dplyr::mutate(Total = base::as.integer(Total)) %>%
      finalize_tbl(digits = digits, caption = caption) %>%
      flextable::add_header_row(values = c("", var1_str, ""),
                                colwidths = c(1, var1_lvls, 1)) %>%
      flextable::vline(j = c(1, var1_lvls + 1),
                       border = officer::fp_border(width = 1.2)) %>%
      flextable::hline(i = 1, part = "header") %>%
      flextable::hline(i = 1, j = c(1, var1_lvls + 2), part = "header",
                       border = officer::fp_border(color = NA)) %>%
      flextable::bold(j = 1)

  }

  else if (row_pct == TRUE) {

    if (base::is.null(caption)) {
      caption <- base::paste("Two-Way Counts (with Row Percentages) of", var1, "vs.", var2)
    }

    mosaic::tally(formula, data = data) %>%
      tibble::as_tibble() %>%
      tidyr::pivot_wider(names_from = {{ var1 }}, values_from = n) %>%
      janitor::adorn_totals("col") %>%
      janitor::adorn_percentages("row") %>%
      janitor::adorn_pct_formatting(digits = 2) %>%
      janitor::adorn_ns(position = "front") %>%
      dplyr::mutate(dplyr::across(-1, ~sub(" ", "\n", .))) %>%
      finalize_tbl(digits = digits, caption = caption) %>%
      flextable::add_header_row(values = c("", var1_str, ""),
                                colwidths = c(1, var1_lvls, 1)) %>%
      flextable::vline(j = c(1, var1_lvls + 1),
                       border = officer::fp_border(width = 1.2)) %>%
      flextable::hline(i = 1, part = "header") %>%
      flextable::hline(i = 1, j = c(1, var1_lvls + 2), part = "header",
                       border = officer::fp_border(color = NA)) %>%
      flextable::bold(j = 1)


  }

  else {
    base::stop("The row percentage argument (`row_pct`) must be either TRUE or FALSE.")
  }
}
