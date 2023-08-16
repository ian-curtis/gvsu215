#' Create a simple bar plot
#'
#' @param data A data frame.
#' @param formula The variable to tabulate. Should be given in formula notation, `~var`.
#' @param type The type of plot to create. Valid options are "percent" (the default) or "count".
#' @param fill The fill of the plot. Valid options are a character color (for one variable plots) or
#'    a variable given in formula notation (`~var`), used to create a grouped bar plot.
#' @param title An override for the title of the plot. A sensible default is provided.
#' @param na_rm Should missing values be removed? Defaults to FALSE.
#' @param ... Extra title arguments passed on to [ggformula::gf_labs()] (which feeds to [ggplot2::ggplot()]).
#'
#' @return A ggplot object. In an interactive session, results are viewable immediately.
#' @export
#'
#' @examples
#' plot_bar(mtcars, ~cyl)
#' plot_bar(mtcars, ~cyl, type = "count")
#' plot_bar(mtcars, ~cyl, type = "percent", fill = "yellowgreen")
#'
#' plot_bar(mtcars, ~cyl, fill = ~gear)
#' plot_bar(mtcars, ~cyl, type = "count", fill = ~gear)
plot_bar <- function(data, formula, type = c("percent", "count"), fill = '#0032A0', title = NULL, na_rm = FALSE, ...) {

  # error catching

  if (base::length(type) == 2) {
    message("No value for type provided, creating a 'Percent' plot.")
  }

  if (base::length(formula) > 2) {
    stop("Too many variables provided. Try entering only one variable in your formula. \n If you are looking for a grouped bar plot, use the `fill` argument.")
  }

  check_test(ggformula::gf_percents(formula, fill = fill))


  # code
  type <- base::match.arg(type)
  var <- formula[[2]]
  str_of_var <- base::deparse(base::substitute(var))

  lvls <- dplyr::pull(data, var)
  big_lvl <- base::max(base::nchar(unique(lvls)), na.rm = TRUE)

  if (big_lvl > 20) {

    data <- data %>%
      dplyr::mutate("{var}" := base::gsub(" ", "\n", {{ var }}))

  }

  # percent plot
  if (type == 'percent') {

    # ungrouped plot (color fill)
    if (base::is.character(fill)) {

      na <- find_na(data, formula)

      if (na_rm == TRUE) {

        data <- data %>%
          dplyr::select({{ var }}) %>%
          stats::na.omit()

      }

      data %>%
        dplyr::mutate("{var}" := base::factor({{ var }})) %>%
        ggformula::gf_percents(formula, fill = fill) %>%
        ggformula::gf_refine(ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)),
                                                limits = c(0, 100))) %>%
        ggformula::gf_labs(title = ifelse(base::is.null(title),
                                          paste0("Simple Bar Percent of ",
                                                 str_of_var), title),
                           subtitle = base::paste("Missing:", na, "|", "NAs Removed:",
                                                  base::ifelse(na_rm == FALSE, "No", "Yes")),
                           y = "Percent", ...) %>%
        finalize_plot()
    } else {
      # grouped plot (variable fill)

      var_na <- find_na(data, formula)
      fill_na <- find_na(data, fill)

      fill_var <- fill[[2]]
      str_of_fill <- base::deparse(base::substitute(fill_var))

      if (na_rm == TRUE) {

        data <- data %>%
          dplyr::select({{ var }}, {{ fill_var }}) %>%
          stats::na.omit()

      }

      data %>%
        dplyr::mutate("{var}" := base::gsub(" ", "\n", {{ var }}),
                      "{fill_var}" := base::factor({{ fill_var }})) %>%
        ggformula::gf_percents(formula,
                               fill = fill,
                               position = ggplot2::position_dodge2(preserve = "single"),
                               denom = ~fill) %>%
        ggformula::gf_labs(title = base::ifelse(
                             base::is.null(title),
                             base::paste("Clustered Bar Graph of", str_of_var, "by", str_of_fill),
                             title),
                           y = "Percent",
                           subtitle = base::paste(str_of_var, "Missing:", var_na, "|",
                                                  str_of_fill, "Missing:", fill_na, "|",
                                                  "NAs Removed:", base::ifelse(na_rm == FALSE, "No", "Yes")),
                           ...) %>%
        ggformula::gf_refine(ggplot2::scale_fill_brewer(palette = "Dark2", na.value = "grey"),
                             ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)))) %>%
        finalize_plot()
    }


  } # count plot
  else if (type == 'count') {

    na <- find_na(data, formula)

    if (na_rm == TRUE) {

      data <- data %>%
        dplyr::select({{ var }}) %>%
        stats::na.omit()

    }

    data %>%
      dplyr::mutate("{var}" := gsub(" ", "\n", {{ var }})) %>%
      ggformula::gf_bar(formula, fill = fill) %>%
      ggformula::gf_refine(ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)))) %>%
      ggformula::gf_labs(title = base::ifelse(base::is.null(title),
                                        paste0("Simple Bar Count of ", str_of_var),
                                        title),
                         subtitle = base::paste("Missing:", na, "|", "NAs Removed:",
                                                base::ifelse(na_rm == FALSE, "No", "Yes")),
                         y = "Count", ...) %>%
      finalize_plot()

  }
}

#' Create a simple boxplot
#'
#' `plot_box()` builds a simple, pre-themed boxplot on one variable alone or grouped by another variable.
#'
#' @inheritParams plot_bar
#' @param formula Variables entered in formula notation. Either `~var` for a one-variable boxplot
#'    or `var1~var2` for a grouped boxplot where `var2` is a grouping variable.
#' @param fill The fill color for the boxplot. Entered as a character.
#'
#' @return A ggplot object. In an interactive environment, results are viewable immediately.
#' @export
#'
#' @examples
#' plot_box(mtcars, ~wt)
#' plot_box(mtcars, wt~gear, fill = 'orangered4')
#' plot_box(mtcars, wt~gear)
plot_box <- function(data, formula, fill = "grey80", title = NULL, na_rm = FALSE, ...) {

  # error catching
  check_test(ggformula::gf_boxplot(formula, data = data, geom = "errorbar", linewidth = 2, width = 0))

  # code
  var <- formula[[2]]
  str_of_var <- base::deparse(base::substitute(var))

  if (base::max(data[, str_of_var]) <= 1000000 & base::max(data[, str_of_var]) >= 0.000001) {

    plot_labels <- function (x) format(x, scientific = FALSE)

  } else {

    plot_labels <- function (x) format(x, scientific = TRUE)

  }

  if (length(formula) == 2) {

    na <- find_na(data, formula)

    if (na_rm == TRUE) {

      data <- data %>%
        dplyr::select({{ var }}) %>%
        stats::na.omit()

    }

    ggformula::gf_boxplot(formula, data = data, geom = "errorbar", linewidth = 2, width = 0) %>%
      ggformula::gf_boxplot(formula, data = data, fill = fill, width = 0.5, lwd = 1, color = "black",
                            outlier.shape = 21, outlier.size = 2.5, outlier.color = "grey70",
                            outlier.fill = "black", notchwidth = 2) %>%
      ggformula::gf_labs(x = str_of_var,
              title = base::ifelse(base::is.null(title),
                                   base::paste("Boxplot of", str_of_var),
                                   title),
              subtitle = base::paste("Missing:", na, "|",
                                     "NAs Removed:", base::ifelse(na_rm == FALSE, "No", "Yes")),
              ...) %>%
      finalize_plot() %>%
      ggformula::gf_refine(ggplot2::scale_x_continuous(labels = plot_labels)) %>%
      ggformula::gf_theme(panel.grid.major.y = ggplot2::element_blank(),
               panel.grid.major.x = ggplot2::element_line(color = "grey70", linewidth = .5),
               axis.text.y = ggplot2::element_blank(),
               axis.text.x = ggplot2::element_text(size = 15)) +
      ggplot2::ylim(-0.4, 0.4)


  } else {

    by_var <- formula[[3]]
    str_of_by <- base::deparse(base::substitute(by_var))

    na <- find_na(data, formula, n = 2)

    if (na_rm == TRUE) {

      data <- data %>%
        dplyr::select({{ var }}, {{ by_var }}) %>%
        stats::na.omit()

    }

    data <- data %>%
      dplyr::mutate("{by_var}" := base::factor({{ by_var }}))

    ggformula::gf_boxplot(formula, data = data, geom = "errorbar", linewidth = 2, width = 0) %>%
      ggformula::gf_boxplot(formula, data = data, fill = fill, width = 0.5, lwd = 1, color = "black",
                            outlier.shape = 21, outlier.size = 2.5, outlier.color = "grey70",
                            outlier.fill = "black", notchwidth = 2) %>%
      ggformula::gf_labs(title = ifelse(base::is.null(title),
                             paste("Boxplot of", str_of_var, "by", str_of_by),
                             title),
                         subtitle = paste(str_of_var, "Missing:", na[[1]], "|",
                                          str_of_by, "Missing:", na[[2]], "|",
                                          "NAs Removed:", base::ifelse(na_rm == FALSE, "No", "Yes")),
                         ...) %>%
      finalize_plot() %>%
      ggformula::gf_refine(ggplot2::scale_y_continuous(labels = plot_labels)) %>%
      ggformula::gf_theme(axis.text.x = ggplot2::element_text(size = 15),
               axis.text.y = ggplot2::element_text(size = 15))

  }

}


#' Create a simple histogram
#'
#' `plot_hist()` builds a histogram on one variable alone or grouped by a single, categorical grouping
#'    variable. The number of grouped plot columns can be adjusted based on the levels of the
#'    grouping variable.
#'
#' @inheritParams plot_bar
#' @param fill The fill color of the bins, entered as a character.
#' @param binwidth The width of the bins, entered as a number.
#' @param group A grouping (faceting) variable entered in formula syntax, `~group_var`.
#' @param group_cols The number of columns to make in a grouped (faceted) plot. Defaults to
#'    1 (stacked vertical plots).
#'
#' @return A ggplot object. In an interactive environment, results are viewable immediately.
#' @export
#'
#' @examples
#' plot_hist(mtcars, ~drat)
#' plot_hist(mtcars, ~drat, binwidth = 0.05)
#' plot_hist(mtcars, ~drat, binwidth = 0.05, fill = "red")
#'
#' plot_hist(mtcars, ~drat, binwidth = 0.05, group = ~cyl)
#' plot_hist(mtcars, ~drat, binwidth = 0.05, group = ~cyl, group_cols = 2)
plot_hist <- function(data, formula, fill = "#0032A0", binwidth = NULL, group = NULL, group_cols = 1, title = NULL, na_rm = FALSE, ...) {

  # error catching
  if (is.null(binwidth)) {

    warning("No value for breaks supplied. Your histogram may not show your data accurately.")

  }

  if (base::length(formula) > 2) {

    stop("Too many variables in formula. Trying for a grouped histogram? Try the `facet` argument.")

  }

  check_test(ggformula::gf_histogram(formula, data = data, type="count", binwidth = binwidth, fill = fill, color = 'grey40', alpha = 100))


  # code
  var <- formula[[2]]
  str_of_var <- base::deparse(base::substitute(var))

  if (base::max(data[, str_of_var]) <= 1000000 & base::max(data[, str_of_var]) >= 0.000001) {

    plot_labels <- function (x) format(x, scientific = FALSE)

  } else {

    plot_labels <- function (x) format(x, scientific = TRUE)

  }


  if (base::is.null(group)) {

    n_na <- find_na(data, formula)

    if (na_rm == TRUE) {

      data <- data %>%
        dplyr::select({{ var }}) %>%
        stats::na.omit()
    }



    ggformula::gf_histogram(formula, data = data, type="count", binwidth = binwidth, fill = fill, color = 'grey40', alpha = 100) %>%
      ggformula::gf_refine(ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)))) %>%
      ggformula::gf_labs(y = "Count",
                         title = base::ifelse(base::is.null(title),
                                        base::paste("Histogram of", str_of_var),
                                        title),
                         subtitle = base::paste("Missing:", n_na, "|", "NAs Removed:",
                                                base::ifelse(na_rm == FALSE, "No", "Yes")),
                         ...) %>%
      finalize_plot() %>%
      ggformula::gf_refine(ggplot2::scale_x_continuous(labels = plot_labels))
  } else {

    facet_var <- group[[2]]
    str_of_facet <- base::deparse(base::substitute(facet_var))

    var_na <- find_na(data, formula)
    facet_na <- find_na(data, group)

    if (na_rm == TRUE) {
      data <- data %>%
        dplyr::select({{ var }}, {{ facet_var }}) %>%
        stats::na.omit()
    }

    ggformula::gf_histogram(formula, data = data, binwidth = binwidth, fill = fill, color = "grey40", alpha = 100) %>%
      ggformula::gf_facet_wrap(group, ncol = group_cols) %>%
      ggformula::gf_refine(ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)))) %>%
      ggformula::gf_labs(y = "Count",
                         title = base::ifelse(base::is.null(title),
                                              base::paste("Histogram of", str_of_var, "by", str_of_facet),
                                              title),
                         subtitle = base::paste(str_of_var, "Missing:", var_na, "|",
                                                str_of_facet, "Missing:", facet_na, "|",
                                                "NAs Removed:", base::ifelse(na_rm == FALSE, "No", "Yes")),
                         ...) %>%
      finalize_plot() %>%
      ggformula::gf_refine(ggplot2::scale_x_continuous(labels = plot_labels)) %>%
      ggformula::gf_theme(panel.border = ggplot2::element_rect(color = "black", fill = NA))

  }

}


#' Create a simple scatterplot
#'
#' `plot_scatter()` creates a pre-themed scatterplot on two variables, optionally grouped by a
#'    third categorical variable.
#'
#' @inheritParams plot_bar
#' @param formula Variables to build the plot on. Should be entered in formula notation, `var1~var2`.
#' @param fill The fill of the plot. Valid options are a character color (for standard scatterplots) or
#'    a variable given in formula notation (`~var`), used to create a grouped scatterplot.
#' @param legend_title The title of the lengend. Ignored in non-grouped plots. Default is "Legend".
#' @param axis_lines Should major axis lines appear on the plot? Valid options are "none" or "both.
#'    Defaults to "none".
#' @param ls_line Should a least squares line (or lines) appear on the plot? Defaults to FALSE.
#'
#' @return A ggplot object. In an interactive environment, results are viewable immediately.
#' @export
#'
#' @examples
#' plot_scatter(mtcars, wt~drat)
#' plot_scatter(mtcars, wt~drat, fill = "red")
#' plot_scatter(mtcars, wt~drat, axis_lines = "both")
#'
#' plot_scatter(mtcars, wt~drat, fill = ~cyl)
#' plot_scatter(mtcars, wt~drat, fill = ~cyl, legend_title = "Cyl")
plot_scatter <- function(data, formula, fill = "#0032a0", title = NULL, legend_title = NULL, axis_lines = c("none", "both"), ls_line = FALSE, ...) {

  # error catching
  message("NAs always removed (in pairs) for scatterplots.")

  if (base::is.character(fill) & !base::is.null(legend_title)) {
    warning("Legend title argument ignored. Legends are only needed for grouped scatterplots.")
  }

  check_test(ggformula::gf_point(formula, data = data, fill = fill, color = "grey80", shape = 21, size = 2))

  # code
  ind_var <- formula[[2]]
  ind_str <- base::deparse(base::substitute(ind_var))

  dep_var <- formula[[3]]
  dep_str <- base::deparse(base::substitute(dep_var))

  n_na <- find_na(data, formula, n = 2)

  axis_lines <- base::match.arg(axis_lines)

  obs_used <- base::nrow(data %>% dplyr::select({{ ind_var }}, {{ dep_var }}) %>% stats::na.omit())

  if (axis_lines == "none") {

    axis_theme <- ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                                 panel.grid.major.x = ggplot2::element_blank())

  } else {

    axis_theme <- ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = 'grey70', linewidth = .5),
                                 panel.grid.major.x = ggplot2::element_line(color = 'grey70', linewidth = .5))

  }

  # non-grouping
  if (base::is.character(fill)) {

    if (ls_line == FALSE) {

      ggformula::gf_point(formula, data = data, fill = fill, color = "grey60", shape = 21, size = 2) %>%
        ggformula::gf_labs(title = base::ifelse(base::is.null(title),
                                                base::paste("Scatterplot of", ind_str, "by", dep_str),
                                                title),
                           subtitle = base::paste(ind_str, "Missing:", n_na[[1]], "|",
                                                  dep_str, "Missing:", n_na[[2]],
                                                  "\nObservations Used:", obs_used, "| NAs Removed: Yes"),
                           ...) %>%
        finalize_plot() %>%
        ggformula::gf_theme(axis_theme)

    } else if (ls_line == TRUE) {

      ggformula::gf_point(formula, data = data, fill = fill, color = "grey60", shape = 21, size = 2) %>%
        ggformula::gf_lm(color = "darkred") %>%
        ggformula::gf_labs(title = base::ifelse(base::is.null(title),
                                                base::paste("Scatterplot of", ind_str, "by", dep_str),
                                                title),
                           subtitle = base::paste(ind_str, "Missing:", n_na[[1]], "|",
                                                  dep_str, "Missing:", n_na[[2]],
                                                  "\nObservations Used:", obs_used, "| NAs Removed: Yes"),
                           ...) %>%
        finalize_plot() %>%
        ggformula::gf_theme(axis_theme)

    }



  } else { # grouped

    group_var <- fill[[2]]
    group_str <- base::deparse(base::substitute(group_var))

    group_na <- find_na(data, fill)

    fctr <- data %>%
      dplyr::select({{ group_var }}) %>%
      base::is.factor()

    if (fctr == FALSE) {

      data <- data %>%
        dplyr::mutate("{group_str}" := base::as.factor({{ group_var }}))

    }

    data <- data %>%
      dplyr::select({{ ind_var }}, {{ dep_var }}, {{ group_var }}) %>%
      stats::na.omit()

    if (ls_line == FALSE) {

    ggformula::gf_point(formula, data = data, fill = fill, color = "grey30", shape = 21, size = 2) %>%
      ggformula::gf_labs(title = ifelse(base::is.null(title),
                                        base::paste0("Grouped Scatterplot of ", ind_str, " and ",
                                                     dep_str, "\n by ", group_str),
                                        title),
                         color = base::ifelse(is.null(legend_title), group_str, legend_title),
                         subtitle = base::paste0(ind_str, " Missing: ", n_na[[1]], " | ",
                                                dep_str, " Missing: ", n_na[[2]], " | ",
                                                group_str, " Missing: ", group_na, "\n",
                                                "Observations Used: ", obs_used, " | NAs Removed: Yes"),
                         ...) %>%
      finalize_plot() %>%
      ggformula::gf_theme(axis_theme) %>%
      gf_refine(ggplot2::scale_fill_viridis_d())

    } else if (ls_line == TRUE) {

      # ggformula::gf_point(formula, data = data, fill = fill, color = "grey80", shape = 21, size = 2) %>%
      #   ggformula::gf_lm(color = fill) %>%
      ggformula::gf_lm(formula, data = data, color = fill) %>%
        gf_point(size = 2) %>%
        ggformula::gf_labs(title = ifelse(base::is.null(title),
                                          base::paste0("Grouped Scatterplot of ", ind_str, " and ",
                                                       dep_str, "\n by ", group_str),
                                          title),
                           color = base::ifelse(is.null(legend_title), group_str, legend_title),
                           subtitle = base::paste0(ind_str, " Missing: ", n_na[[1]], " | ",
                                                   dep_str, " Missing: ", n_na[[2]], " | ",
                                                   group_str, " Missing: ", group_na, "\n",
                                                   "Observations Used: ", obs_used, " | NAs Removed: Yes"),
                           ...) %>%
        finalize_plot() %>%
        ggformula::gf_theme(axis_theme) %>%
        gf_refine(ggplot2::scale_color_viridis_d()) %>%
        gf_point(color = "grey30", shape = 21, size = 2)

    }



  }

}
