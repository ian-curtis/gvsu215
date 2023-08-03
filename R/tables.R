#' Create a one way data table
#'
#' `one_way()` creates a table giving counts and percentages on one variable.
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
#' one_way(mtcars, ~gear)
#' one_way(mtcars, ~gear, digits = 2)
#'
#' # Will give an error
#' try(one_way(mtcars, ~Gear))
one_way <- function(data, formula, digits = 3, caption = NULL) {

  # error catching
  check_test(mosaic::tally(formula, data = data, format = "count"))


  # code
  var <- formula[[2]]
  str_of_var <- base::deparse(base::substitute(var))

  if (base::is.null(caption)) {
    caption <- base::paste("One Way Counts on Variable", str_of_var)
  }

  mosaic::tally(formula, data = data, format = "count") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(pct = (n / base::nrow(data))*100) %>%
    janitor::adorn_totals("row") %>%
    finalize_tbl(digits = digits, caption = base::paste("One Way Counts on Variable", str_of_var)) %>%
    flextable::set_header_labels(n = "Count", pct = "Percent") %>%
    flextable::set_caption(caption = flextable::as_paragraph(flextable::as_chunk(caption)))

}


#' Create a two way data table
#'
#' @inheritParams one_way
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
#' two_way(mtcars, cyl~gear)
#' two_way(mtcars, cyl~gear, row_pct = TRUE)
#' two_way(mtcars, cyl~gear, caption = "This is the new caption")
#'
#' # Will give an error
#' try(two_way(mtcars, Cyl~Gear))
two_way <- function(data, formula, row_pct = FALSE, digits = 3, caption = NULL) {

  # error catching
  check_test(mosaic::tally(formula, data = data))

  # code
  dep_var <- formula[[2]]
  ind_var <- formula[[3]]
  dep_str <- base::deparse(base::substitute(ind_var))
  ind_str <- base::deparse(base::substitute(dep_var))

  if (row_pct == FALSE) {

    if (base::is.null(caption)) {
      caption <- base::paste("Two-Way Counts of", ind_var, "vs.", dep_var)
    }

    mosaic::tally(formula, data = data) %>%
      tibble::as_tibble() %>%
      tidyr::pivot_wider(names_from = {{ dep_var }}, values_from = n) %>%
      janitor::adorn_totals("col") %>%
      dplyr::mutate(Total = base::as.integer(Total)) %>%
      finalize_tbl(digits = digits, caption = caption) #%>%
    # flextable::set_header_labels()

    # kableExtra::kbl(digits = digits, caption = paste("Two-Way Counts of", ind_var, "vs.", dep_var)) %>%
    # kableExtra::kable_styling(c('striped', 'bordered', 'condensed'), full_width = F) %>%
    # kableExtra::add_header_above(c("", setNames(7, dep_str), "")) %>%
    # kableExtra::row_spec(0, extra_css = "border-bottom: 3px solid black")
  }

  else if (row_pct == TRUE) {

    if (base::is.null(caption)) {
      caption <- base::paste("Two-Way Counts (with Row Percentages) of", ind_var, "vs.", dep_var)
    }

    mosaic::tally(formula, data = data) %>%
      tibble::as_tibble() %>%
      tidyr::pivot_wider(names_from = {{ dep_var }}, values_from = n) %>%
      janitor::adorn_totals("col") %>%
      janitor::adorn_percentages("row") %>%
      janitor::adorn_pct_formatting(digits = 2) %>%
      janitor::adorn_ns(position = "front") %>%
      dplyr::mutate(dplyr::across(-1, ~sub(" ", "\n", .))) %>%
      finalize_tbl(digits = digits, caption = caption)
    # kableExtra::kbl(digits = digits, caption = paste("Two-Way Counts (with Row Percentages) of", ind_var, "vs.", dep_var)) %>%
    # kableExtra::kable_styling(c('striped', 'bordered', 'condensed'), full_width = F) %>%
    # kableExtra::add_header_above(c("", setNames(7, dep_str), "")) %>%
    # kableExtra::row_spec(0, extra_css = "border-bottom: 3px solid black")

  }

  else {
    base::stop("The row percentage argument (`row_pct`) must be either TRUE or FALSE.")
  }
}
