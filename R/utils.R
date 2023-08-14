#' Add default theme options to tables
#'
#' `finalize_tbl()` is used internally to complete the table creation process. This function creates
#'    a flextable object and adds theme choices and size specifications.
#'
#' @param table A dataframe (to be coerced to a flextable object)
#' @param digits The number of digits to round double columns to
#' @param striped Should the table be striped alternating colors (white/grey)? Defaults to TRUE.
#' @param caption The table caption. Defaults to NULL (or no caption).
#' @param na_str What string should appear when an NA appears? Defaults to "NA".
#'
#' @return An object of class flextable.
#' @export
#'
#' @examples
#' finalize_tbl(mtcars, digits = 3, caption = "The mtcars Dataset")
finalize_tbl <- function(table, digits, striped = TRUE, caption = NULL, na_str = "NA") {

  big_border <- officer::fp_border(color = "#e1e4e5", width = 1)

  if (striped == FALSE) {
    table %>%
      flextable::flextable() %>%
      flextable::bg(bg = "#ffffff", part = "all") %>%
      flextable::colformat_double(digits = digits, na_str = na_str) %>%
      flextable::colformat_char(na_str = na_str) %>%
      flextable::colformat_lgl(na_str = na_str) %>%
      flextable::border_outer(part="all", border = big_border ) %>%
      flextable::hline(i = 1, border = officer::fp_border(color = "#000000", style = "solid", width = 2), part = "header") %>%
      flextable::vline(border = officer::fp_border(color = '#e1e4e5', width = 1)) %>%
      flextable::bold(bold = TRUE, part = "header") %>%
      flextable::align(align = "center", part = "header") %>%
      flextable::autofit() %>%
      flextable::fit_to_width(7.5) %>%
      flextable::set_caption(caption = flextable::as_paragraph(flextable::as_chunk(caption,
                                                             props = officer::fp_text(font.family = "Helvetica",
                                                                                      bold = FALSE))))
  }

  else if (striped == TRUE) {
    table %>%
      flextable::flextable() %>%
      flextable::colformat_double(digits = digits, na_str = na_str) %>%
      flextable::colformat_char(na_str = na_str) %>%
      flextable::colformat_lgl(na_str = na_str) %>%
      flextable::theme_zebra(odd_header = "#ffffff", even_body = "#ffffff") %>%
      flextable::border_outer(part="all", border = big_border ) %>%
      flextable::hline(border = officer::fp_border(color = "#000000", style = "solid", width = 2), part = "header") %>%
      flextable::vline(border = officer::fp_border(color = '#e1e4e5', width = 1)) %>%
      flextable::bold(bold = TRUE, part = "header") %>%
      flextable::align(align = "center", part = "header") %>%
      flextable::autofit() %>%
      flextable::fit_to_width(7.5) %>%
      flextable::set_caption(caption = flextable::as_paragraph(flextable::as_chunk(caption,
                                                             props = officer::fp_text(font.family = "Helvetica", bold = FALSE))))
  }

}

#' Add default theme options to plots
#'
#' `finalize_plot()` is used internally to complete the plot creation process. This function uses `ggformula`
#'    to create a `ggplot` object and adds theme choices and size specifications.
#'
#' @param plot A plot (to be piped into `ggformula::gf_theme`)
#'
#' @return A `ggplot` object..
#' @export
#'
#' @examples
#' library(ggformula)
#'
#' gf_point(mpg~hp, data = mtcars) %>%
#'   finalize_plot()
finalize_plot <- function(plot) {

  plot %>%
    ggformula::gf_theme(ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                              axis.ticks.y = ggplot2::element_blank(),
                              panel.grid.major.x = ggplot2::element_blank(),
                              panel.grid.minor.y = ggplot2::element_blank(),
                              panel.grid.major.y = ggplot2::element_line(color = 'grey70', linewidth = .5),
                              panel.background = ggplot2::element_rect(fill = 'white'),
                              axis.line.y = ggplot2::element_line(color = 'black'),
                              axis.line.x = ggplot2::element_line(color = 'black'),
                              plot.title = ggplot2::element_text(face = "bold"),
                              axis.title.y = ggplot2::element_text(face = "bold"),
                              axis.title.x = ggplot2::element_text(face = "bold"),
                              legend.key = ggplot2::element_rect(fill = 'white')))

}

#' Find missing values
#'
#' Used internally to calculate the number of missing values in one or two variables.
#'
#' @param data A data frame
#' @param formula One or two variables in formula notation: `~var1` or `var1 ~ var2`
#' @param n How many variables to analyze. Acceptable values are 1 or 2.
#'
#' @return An number (n = 1) or a list of two numbers (n = 2)
#' @export
#'
#' @examples
#' find_na(mtcars, ~cyl)
#' find_na(mtcars, wt~cyl, n = 2)
find_na <- function(data, formula, n = 1) {

  if (n == 1) {

    var <- formula[[2]]

    n_na <- data %>%
      dplyr::filter(base::is.na({{ var }})) %>%
      base::nrow()

    return(n_na)

  } else if (n == 2) {

    var1 <- formula[[2]]
    var2 <- formula[[3]]

    var1_na <- data %>%
      dplyr::filter(base::is.na({{ var1 }})) %>%
      base::nrow()

    var2_na <- data %>%
      dplyr::filter(base::is.na({{ var2 }})) %>%
      base::nrow()

    return(list(var1_na, var2_na))

  }

}

#' Check if a confidence level is entered in decimal form
#'
#' Used internally to detect if a user inputted a confidence level correctly (i.e., not in quotes
#' and as a decimal).
#'
#' @param level The confidence level in question
#'
#' @return Nothing, or an error message.
#' @export
#'
#' @examples
#' check_conf_lvl(.95)
#' try(check_conf_lvl("95%"))
#' try(check_conf_lvl(95))
check_conf_lvl <- function(level) {

  if (level > 1 | level < 0 | base::is.character(level)) {

    stop("Confidence level should be given as a decimal between 0 and 1, inclusive (not in quotes).")

  }

}

#' Check if an error will occur later
#'
#' Used internally to check if an error might occur later. We catch the error here to provide
#' a more helpful message to users based on the most common cause of errors.
#'
#' @param code The code to try to run
#'
#' @return Nothing, or an error message.
#' @export
#'
#' @examples
#' library(stats)
#'
#' check_test(head(mtcars))
#' try(check_test(head(Mtcars)))
#'
#' check_test(lm(wt~drat, data = mtcars))
#' try(check_test(lm(Wt~drat, data = mtcars)))
check_test <- function(code) {

  base::tryCatch(code,
                 error = function (e) stop("Could not complete the process. Perhaps you spelled the dataset name or a variable name wrong? Hint: R is case-sensitive.")
  )

}
