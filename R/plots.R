#' Create a simple bar plot
#'
#' @param data A data frame.
#' @param formula The variable to tabulate. Should be given in formula notation, `~var`.
#' @param type The type of plot to create. Valid options are "percent" (the default) or "count".
#' @param fill The fill of the plot. Valid options are a character color (for one variable plots) or
#'    a variable given in formula notation (`~var`), used to create a grouped bar plot.
#' @param layout The bar type for grouped plots. Either `"sbs"` for side-by-side bars or `"stack"` for stacked bars. `fill` must be a variable for this to go into effect.
#' @param orient The orientation for the plot (either "vertical", the default, or "horizontal"). As a
#'   shortcut, "v" and "h" may be used.
#' @param dodge The number of rows to dodge the axis labels to, should they be overlapping.
#' @param title An override for the title of the plot. A sensible default is provided.
#' @param subtitle A switch for hiding the default subtitle. One of "show" or "hide".
#' @param na_rm Should missing values be removed? Defaults to TRUE.
#' @param ... Extra title arguments passed on to [ggformula::gf_labs()] (which feeds to [ggplot2::ggplot()]).
#'
#' @return A ggplot object. In an interactive session, results are viewable immediately.
#' @export
#'
#' @examples
#' plot_bar(mtcars, ~cyl)
#' plot_bar(mtcars, ~cyl, type = "count")
#' plot_bar(mtcars, ~cyl, type = "percent", fill = "yellowgreen")
#' plot_bar(mtcars, ~cyl, orient = "horizontal")
#' plot_bar(dplyr::starwars, ~hair_color, dodge = 2)
#' plot_bar(mtcars, ~cyl, subtitle = "hide")
#'
#' plot_bar(mtcars, ~cyl, fill = ~gear)
#' plot_bar(mtcars, ~cyl, fill = ~gear, layout = "stack")
#' plot_bar(mtcars, ~cyl, type = "count", fill = ~gear)
plot_bar <- function(data, formula, type = c("percent", "count"), fill = '#0032A0', layout = c("sbs", "stack"), orient = c("vertical", "horizontal"), dodge = 1, title = NULL, subtitle = c("show", "hide"), na_rm = TRUE, ...) {

  # check for empty strings and make them actual NAs
  data <- tibble::as_tibble(data) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., "")))

  # error catching

  if (base::length(type) == 2) {
    rlang::inform("When no value for `type` is provided, a 'percent' plot is created.",
                  .frequency = "regularly",
                  .frequency_id = "bar-type")
  }

  if (base::length(formula) > 2) {
    cli::cli_abort("Too many variables provided. Try entering only one variable in your formula.\nIf you are looking for a grouped bar plot, use the {.var fill} argument.")
  }

  check_test(ggformula::gf_percents(formula, fill = fill))


  # code
  type <- base::match.arg(type)
  orient <- base::match.arg(orient)
  subtitle_display <- base::match.arg(subtitle)
  var <- formula[[2]]
  var_str <- base::deparse(base::substitute(var))
  layout <- base::match.arg(layout)

  if (base::is.character(fill) & layout == "stack") {
    cli::cli_warn("{.var fill} must be a variable (not a character) for {.var layout} to have an effect.")
  }

  # find the levels of the variable
  lvls <- dplyr::pull(data, var)
  # find length of the biggest level
  big_lvl <- base::max(base::nchar(as.character(unique(lvls))), na.rm = TRUE)

  # if the biggest level has more than 20 characters, add new lines at each space
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

      plot <- data %>%
        dplyr::mutate("{var}" := base::factor({{ var }})) %>%
        ggformula::gf_percents(formula, fill = fill) %>%
        ggformula::gf_refine(ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)))) %>%
        ggformula::gf_labs(title = ifelse(base::is.null(title),
                                          paste0("Bar Chart (Percents) of ",
                                                 var_str), title),
                           subtitle = ,
                           y = "Percent", ...) %>%
        ggformula::gf_refine(ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = dodge))) %>%
        finalize_plot()

      if (subtitle_display == "show") {
        plot <- plot + ggplot2::labs(subtitle = base::paste("Missing:", na, "|", "NAs Removed:",
                                                  base::ifelse(na_rm == FALSE, "No", "Yes")))
      }

      if (orient == "vertical") return(plot) else return(plot + ggplot2::coord_flip())


    } else {
      # grouped plot (variable fill)

      var_na <- find_na(data, formula)
      fill_na <- find_na(data, fill)

      fill_var <- fill[[2]]
      fill_str <- base::deparse(base::substitute(fill_var))

      if (na_rm == TRUE) {

        data <- data %>%
          dplyr::select({{ var }}, {{ fill_var }}) %>%
          stats::na.omit()

      }

      plot <- data %>%
        dplyr::mutate("{var}" := base::gsub(" ", "\n", {{ var }}),
                      "{fill_var}" := base::factor({{ fill_var }})) %>%
        ggformula::gf_percents(formula,
                               fill = fill,
                               width = 0.5,
                               position = if(layout == "sbs") ggplot2::position_dodge2(preserve = "single") else "stack",
                               denom = ~fill) %>%
        ggformula::gf_labs(title = base::ifelse(
                             base::is.null(title),
                             base::paste("Clustered Bar Chart (Percents) of", var_str, "by", fill_str),
                             title),
                           y = "Percent",
                           ...) %>%
        ggformula::gf_refine(ggplot2::scale_fill_brewer(palette = "Dark2", na.value = "grey"),
                             ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1))),
                             ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = dodge))) %>%
        finalize_plot()

      if (subtitle_display == "show") {
        plot <- plot + ggplot2::labs(subtitle = base::paste(var_str, "Missing:", var_na, "|",
                                                   fill_str, "Missing:", fill_na, "|",
                                                   "NAs Removed:", base::ifelse(na_rm == FALSE,
                                                                                "No",
                                                                                "Yes")))
      }

      if (orient == "vertical") return(plot) else return(plot + ggplot2::coord_flip())
    }


  } # count plot
  else if (type == 'count') {

    if (base::is.character(fill)) {
      # one-var count (color fill)

      na <- find_na(data, formula)

      if (na_rm == TRUE) {

        data <- data %>%
          dplyr::select({{ var }}) %>%
          stats::na.omit()

      }

      plot <- data %>%
        dplyr::mutate("{var}" := base::factor({{ var }})) %>%
        ggformula::gf_counts(formula, fill = fill) %>%
        ggformula::gf_refine(ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)))) %>%
        ggformula::gf_labs(title = ifelse(base::is.null(title),
                                          paste0("Bar Chart (Counts) of ",
                                                 var_str), title),
                           y = "Count", ...) %>%
        ggformula::gf_refine(ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = dodge))) %>%
        finalize_plot()

      if (subtitle_display == "show") {
        plot <- plot + ggplot2::labs(subtitle = base::paste("Missing:", na, "|", "NAs Removed:",
                                                   base::ifelse(na_rm == FALSE, "No", "Yes")))
      }

      if (orient == "vertical") return(plot) else return(plot + ggplot2::coord_flip())

    } else {
      # grouped count (variable fill)

      var_na <- find_na(data, formula)
      fill_na <- find_na(data, fill)

      fill_var <- fill[[2]]
      fill_str <- base::deparse(base::substitute(fill_var))

      if (na_rm == TRUE) {

        data <- data %>%
          dplyr::select({{ var }}, {{ fill_var }}) %>%
          stats::na.omit()

      }

      plot <- data %>%
        dplyr::mutate("{var}" := base::gsub(" ", "\n", {{ var }}),
                      "{fill_var}" := base::factor({{ fill_var }})) %>%
        ggformula::gf_counts(formula,
                             fill = fill,
                             width = 0.5,
                             position = if(layout == "sbs") ggplot2::position_dodge2(preserve = "single") else "stack") %>%
        ggformula::gf_labs(title = base::ifelse(
          base::is.null(title),
          base::paste("Clustered Bar Chart (Counts) of", var_str, "by", fill_str),
          title),
          y = "Count",
          subtitle = base::paste(var_str, "Missing:", var_na, "|",
                                 fill_str, "Missing:", fill_na, "|",
                                 "NAs Removed:", base::ifelse(na_rm == FALSE, "No", "Yes")),
          ...) %>%
        ggformula::gf_refine(ggplot2::scale_fill_brewer(palette = "Dark2", na.value = "grey"),
                             ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1))),
                             ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = dodge))) %>%
        finalize_plot()

      if (subtitle_display == "show") {
        plot <- plot + ggplot2::labs(subtitle = base::paste(var_str, "Missing:", var_na, "|",
                                                   fill_str, "Missing:", fill_na, "|",
                                                   "NAs Removed:", base::ifelse(na_rm == FALSE,
                                                                                "No",
                                                                                "Yes")))
      }

      if (orient == "vertical") return(plot) else return(plot + ggplot2::coord_flip())
    }
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
#' @param breaks A vector of length 3 (start, stop, step) specifying how the x-scale should be broken up.
#'
#' @return A ggplot object. In an interactive environment, results are viewable immediately.
#' @export
#'
#' @examples
#' plot_box(mtcars, ~wt)
#' plot_box(mtcars, wt~gear, fill = 'orangered4')
#' plot_box(mtcars, ~wt, breaks = seq(1, 6, 0.5))
#' plot_box(mtcars, wt~gear)
#' plot_box(mtcars, wt~gear, breaks = seq(1, 6, 0.5))
plot_box <- function(data, formula, fill = "grey80", breaks = NULL, orient = c("vertical", "horizontal"), title = NULL, subtitle = c("show", "hide"), ...) {

  # check for empty strings and make them actual NAs
  data <- tibble::as_tibble(data) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., "")))

  # error catching
  check_test(ggformula::gf_boxplot(formula, data = data, fill = fill))

  rlang::inform("Note: NAs always removed for boxplots", .frequency = "once", .frequency_id = "box-nas")

  # code
  orient <- base::match.arg(orient)
  subtitle_display <- base::match.arg(subtitle)
  var <- formula[[2]]
  var_str <- base::deparse(base::substitute(var))
  px <- base::pretty(data[[var_str]])

  # if the data values are between 0.000001 and 1,000,000: use the raw values
  # otherwise use scientific notation
  if (base::max(data[, var_str], na.rm = TRUE) <= 1000000 & base::max(data[, var_str], na.rm = TRUE) >= 0.000001) {

    plot_labels <- function (x) format(x, scientific = FALSE)

  } else {

    plot_labels <- function (x) format(x, scientific = TRUE)

  }

  if (length(formula) == 2) { # single boxplot

    na <- find_na(data, formula)

    data <- data %>%
      dplyr::select({{ var }}) %>%
      stats::na.omit()

    plot <- ggformula::gf_boxplot(formula, data = data, geom = "errorbar", linewidth = 1.5, width = 0) %>%
      ggformula::gf_boxplot(formula, data = data, fill = fill, width = 0.5, linewidth = 1, color = "black",
                            outlier.shape = 21, outlier.size = 2.5, outlier.color = "grey70",
                            outlier.fill = "black") %>%
      ggformula::gf_labs(x = var_str,
              title = base::ifelse(base::is.null(title),
                                   base::paste("Boxplot of", var_str),
                                   title),
              ...) %>%
      finalize_plot() %>%
      ggformula::gf_theme(panel.grid.major.y = ggplot2::element_blank(),
               panel.grid.major.x = ggplot2::element_line(color = "grey70", linewidth = .5),
               axis.text.y = ggplot2::element_blank()) %>%
      ggformula::gf_refine(ggplot2::ylim(-0.4, 0.4))

      if (base::is.null(breaks)) {
        plot <- plot %>%
          ggformula::gf_refine(ggplot2::scale_x_continuous(labels = plot_labels,
                                                           breaks = px,
                                                           limits = range(px)))
      } else {
        plot <- plot %>%
          ggformula::gf_refine(ggplot2::scale_x_continuous(labels = plot_labels,
                                                           breaks = breaks,
                                                           limits = range(px)))
      }

    if (subtitle_display == "show") {
      plot <- plot + ggplot2::labs(subtitle = base::paste("Missing:", na, "|",
                                                 "NAs Removed: Yes"))
    }

    if (orient == "vertical") return(plot) else return(plot + ggplot2::coord_flip())


  } else { # grouped boxplot

    by_var <- formula[[3]]
    by_str <- base::deparse(base::substitute(by_var))

    na <- find_na(data, formula, n = 2)

    data <- data %>%
      dplyr::select({{ var }}, {{ by_var }}) %>%
      stats::na.omit() %>%
      dplyr::mutate("{by_var}" := base::factor({{ by_var }}))

    plot <- ggformula::gf_boxplot(formula, data = data, geom = "errorbar", linewidth = 2, width = 0) %>%
      ggformula::gf_boxplot(formula, data = data, fill = fill, width = 0.5, lwd = 1, color = "black",
                            outlier.shape = 21, outlier.size = 2.5, outlier.color = "grey70",
                            outlier.fill = "black", notchwidth = 2) %>%
      ggformula::gf_labs(title = ifelse(base::is.null(title),
                             paste("Boxplot of", var_str, "by", by_str),
                             title),
                         ...) %>%
      finalize_plot()

    if (base::is.null(breaks)) {

      plot <- plot %>%
        ggformula::gf_refine(ggplot2::scale_y_continuous(labels = plot_labels,
                                                         breaks = px,
                                                         limits = range(px)))

    } else {

      plot <- plot %>%
        ggformula::gf_refine(ggplot2::scale_y_continuous(labels = plot_labels,
                                                         breaks = breaks,
                                                         limits = range(px)))

    }

    if (subtitle_display == "show") {
      plot <- plot + ggplot2::labs(subtitle = paste(var_str, "Missing:", na[[1]], "|",
                                           by_str, "Missing:", na[[2]], "|",
                                           "NAs Removed: Yes"))
    }

    if (orient == "vertical") return(plot) else return(plot + ggplot2::coord_flip())

  }

}


#' Create a simple histogram
#'
#' @description
#' `plot_hist()` builds a histogram on one variable alone or grouped by a single, categorical grouping
#'    variable. The number of grouped plot columns can be adjusted based on the levels of the
#'    grouping variable.
#'
#'  Default histograms will likely not show your data in the best way (the function will warn you of this
#'    once per session). To remedy this, use the `breaks` argument. The values given in the vector should
#'    all be numbers. The first number is the minimum value you want displayed, the second is the maximum
#'    value to display, and the third is the "step" distance.
#'
#'
#' @inheritParams plot_bar
#' @param fill The fill color of the bins, entered as a character.
#' @param breaks A use of the `seq` function (start, stop, step) specifying how the x-scale should be broken up.
#'   A good default is provided based on the range and values of the data.
#' @param group A grouping (faceting) variable entered in formula syntax, `~group_var`.
#' @param group_cols The number of columns to make in a grouped (faceted) plot. Defaults to
#'    1 (stacked vertical plots).
#'
#' @return A ggplot object. In an interactive environment, results are viewable immediately.
#' @export
#'
#' @examples
#' plot_hist(mtcars, ~drat)
#' plot_hist(mtcars, ~drat, breaks = seq(3, 6, 0.5))
#' plot_hist(mtcars, ~drat, breaks = seq(1, 6, 0.5), fill = "red")
#'
#' plot_hist(mtcars, ~drat, group = ~cyl, breaks = seq(2, 5, 0.25))
#' plot_hist(mtcars, ~drat, group = ~cyl, breaks = seq(2, 5, 0.25), group_cols = 2)
plot_hist <- function(data, formula, fill = "#0032A0", breaks = NULL, group = NULL, group_cols = 1, title = NULL, subtitle = c("show", "hide"), ...) {

  # check for empty strings and make them actual NAs
  data <- tibble::as_tibble(data) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., "")))

  subtitle_display <- base::match.arg(subtitle)

  # error catching
  if (is.null(breaks)) {

    cli::cli_alert_warning("No value for breaks supplied. Your histogram may not show your data accurately.")

  }



  if (base::length(formula) > 2) {

    cli::cli_abort("Too many variables in formula. Trying for a grouped histogram? Use the {.var group} argument.")

  }

  rlang::inform("Note: NAs always removed for histograms", .frequency = "once", .frequency_id = "hist-nas")

  check_test(ggformula::gf_histogram(formula, data = data, type="count", breaks = breaks, fill = fill, color = 'grey40', alpha = 100))


  # code
  var <- formula[[2]]
  var_str <- base::deparse(base::substitute(var))

  # if the data values are between 0.000001 and 1,000,000: use the raw values
  # otherwise use scientific notation
  if (base::max(data[, var_str], na.rm = TRUE) <= 1000000 & base::max(data[, var_str], na.rm = TRUE) >= 0.000001) {

    plot_labels <- function (x) format(x, scientific = FALSE)

  } else {

    plot_labels <- function (x) format(x, scientific = TRUE)

  }


  if (base::is.null(group)) { # one-var histogram

    n_na <- find_na(data, formula)

    data <- data %>%
      dplyr::select({{ var }}) %>%
      stats::na.omit()

    if (is.null(breaks)) {

      plot <- ggformula::gf_histogram(formula, data = data, type="count", breaks = breaks, fill = fill, color = 'grey40', alpha = 100) %>%
        ggformula::gf_refine(ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)))) %>%
        ggformula::gf_labs(y = "Count",
                           title = base::ifelse(base::is.null(title),
                                                base::paste("Histogram of", var_str),
                                                title),
                           ...) %>%
        finalize_plot() %>%
        ggformula::gf_refine(ggplot2::scale_x_continuous(labels = plot_labels, guide = ggplot2::guide_axis(check.overlap = TRUE)))

      if (subtitle_display == "show") {
        plot <- plot + ggplot2::labs(subtitle = base::paste("Missing:", n_na, "|", "NAs Removed: Yes"))
      }
      return(plot)

    } else {

      plot <- ggformula::gf_histogram(formula, data = data, type="count", breaks = breaks, fill = fill, color = 'grey40', alpha = 100) %>%
        ggformula::gf_refine(ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)))) %>%
        ggformula::gf_labs(y = "Count",
                           title = base::ifelse(base::is.null(title),
                                                base::paste("Histogram of", var_str),
                                                title),
                           ...) %>%
        finalize_plot() %>%
        ggformula::gf_refine(ggplot2::scale_x_continuous(labels = plot_labels, breaks = breaks, guide = ggplot2::guide_axis(check.overlap = TRUE)))


      if (subtitle_display == "show") {
        plot <- plot + ggplot2::labs(subtitle = base::paste("Missing:", n_na, "|", "NAs Removed: Yes"))
      }

      return(plot)

    }




  } else {
    # grouped histogram

    facet_var <- group[[2]]
    facet_str <- base::deparse(base::substitute(facet_var))

    var_na <- find_na(data, formula)
    facet_na <- find_na(data, group)

    data <- data %>%
      dplyr::select({{ var }}, {{ facet_var }}) %>%
      stats::na.omit()

    if (base::is.null(breaks)) { # build with no breaks

      plot <- ggformula::gf_histogram(formula, data = data, breaks = breaks, fill = fill, color = "grey40", alpha = 100) %>%
        ggformula::gf_facet_wrap(group, ncol = group_cols) %>%
        ggformula::gf_refine(ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)))) %>%
        ggformula::gf_labs(y = "Count",
                           title = base::ifelse(base::is.null(title),
                                                base::paste("Histogram of", var_str, "by", facet_str),
                                                title),
                           ...) %>%
        finalize_plot() %>%
        ggformula::gf_refine(ggplot2::scale_x_continuous(labels = plot_labels, guide = ggplot2::guide_axis(check.overlap = TRUE))) %>%
        ggformula::gf_theme(panel.border = ggplot2::element_rect(color = "black", fill = NA))

      if (subtitle_display == "show") {
        plot <- plot + ggplot2::labs(subtitle = base::paste(var_str, "Missing:", var_na, "|",
                                                            facet_str, "Missing:", facet_na, "|",
                                                            "NAs Removed: Yes"))
      }

      return(plot)

    } else { # build with user-supplied breaks

      plot <- ggformula::gf_histogram(formula, data = data, breaks = breaks, fill = fill, color = "grey40", alpha = 100) %>%
        ggformula::gf_facet_wrap(group, ncol = group_cols) %>%
        ggformula::gf_refine(ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)))) %>%
        ggformula::gf_labs(y = "Count",
                           title = base::ifelse(base::is.null(title),
                                                base::paste("Histogram of", var_str, "by", facet_str),
                                                title),
                           ...) %>%
        finalize_plot() %>%
        ggformula::gf_refine(ggplot2::scale_x_continuous(labels = plot_labels,
                                                         breaks = breaks,
                                                         guide = ggplot2::guide_axis(check.overlap = TRUE))) %>%
        ggformula::gf_theme(panel.border = ggplot2::element_rect(color = "black", fill = NA))

      if (subtitle_display == "show") {
        plot <- plot + ggplot2::labs(subtitle = base::paste(var_str, "Missing:", var_na, "|",
                                                            facet_str, "Missing:", facet_na, "|",
                                                            "NAs Removed: Yes"))
      }

      return(plot)

    }



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
#' @param legend_title The title of the lengend. Ignored in non-grouped plots. Default is the variable name.
#' @param axis_lines Should major axis lines appear on the plot? Valid options are "none" or "both.
#'    Defaults to "none".
#' @param ls_line Should a least squares line (or lines) appear on the plot? Defaults to "hide".
#'
#' @return A ggplot object. In an interactive environment, results are viewable immediately.
#' @export
#'
#' @examples
#' plot_scatter(mtcars, wt~drat)
#' plot_scatter(mtcars, wt~drat, fill = "red")
#' plot_scatter(mtcars, wt~drat, axis_lines = "both")
#' plot_scatter(mtcars, wt~drat, ls_line = "show")
#'
#' plot_scatter(mtcars, wt~drat, fill = ~cyl)
#' plot_scatter(mtcars, wt~drat, fill = ~cyl, ls_line = "show")
#' plot_scatter(mtcars, wt~drat, fill = ~cyl, legend_title = "Cylinders")
plot_scatter <- function(data, formula, fill = "#0032a0", title = NULL, subtitle = c("show", "hide"), legend_title = NULL, axis_lines = c("none", "both"), ls_line = c("hide", "show"), ...) {

  # check for empty strings and make them actual NAs
  data <- tibble::as_tibble(data) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., "")))

  # error catching
  rlang::inform("Note: NAs always removed (in pairs) for scatterplots.", .frequency = "regularly", .frequency_id = "scatter_nas")

  if (base::is.character(fill) & !base::is.null(legend_title)) {
    cli::cli_alert_info("Legend title argument ignored. Legends are only needed for grouped scatterplots.")
  }

  check_test(ggformula::gf_point(formula, data = data, fill = fill, color = "grey80", shape = 21, size = 2))

  # code
  subtitle_display <- base::match.arg(subtitle)
  var1 <- formula[[2]]
  var1_str <- base::deparse(base::substitute(var1))

  var2 <- formula[[3]]
  var2_str <- base::deparse(base::substitute(var2))

  n_na <- find_na(data, formula, n = 2)

  axis_lines <- base::match.arg(axis_lines)
  ls_line <- base::match.arg(ls_line)

  obs_used <- base::nrow(data %>% dplyr::select({{ var1 }}, {{ var2 }}) %>% stats::na.omit())

  # set theme for both axis lines or no axis lines
  if (axis_lines == "none") {

    axis_theme <- ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                                 panel.grid.major.x = ggplot2::element_blank())

  } else {

    axis_theme <- ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = 'grey70', linewidth = .5),
                                 panel.grid.major.x = ggplot2::element_line(color = 'grey70', linewidth = .5))

  }

  # non-grouping
  if (base::is.character(fill)) {

    if (ls_line == "hide") {

      plot <- ggformula::gf_point(formula, data = data, fill = fill, color = "grey60", shape = 21, size = 2) %>%
        ggformula::gf_labs(title = base::ifelse(base::is.null(title),
                                                base::paste("Scatterplot of", var1_str, "by", var2_str),
                                                title),
                           ...) %>%
        finalize_plot() %>%
        ggformula::gf_theme(axis_theme)

      if (subtitle_display == "show") {
        plot <- plot + ggplot2::labs(subtitle = base::paste(var1_str, "Missing:", n_na[[1]], "|",
                                                            var2_str, "Missing:", n_na[[2]],
                                                            "\nObservations Used:", obs_used, "| NAs Removed: Yes"))
      }

      return(plot)

    } else if (ls_line == "show") {

      plot <- ggformula::gf_point(formula, data = data, fill = fill, color = "grey60", shape = 21, size = 2) %>%
        ggformula::gf_lm(color = "darkred") %>%
        ggformula::gf_labs(title = base::ifelse(base::is.null(title),
                                                base::paste("Scatterplot of", var1_str, "by", var2_str),
                                                title),
                           ...) %>%
        finalize_plot() %>%
        ggformula::gf_theme(axis_theme)

      if (subtitle_display == "show") {
        plot <- plot + ggplot2::labs(subtitle = base::paste(var1_str, "Missing:", n_na[[1]], "|",
                                                            var2_str, "Missing:", n_na[[2]],
                                                            "\nObservations Used:", obs_used, "| NAs Removed: Yes"))
      }

      return(plot)

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
      dplyr::select({{ var1 }}, {{ var2 }}, {{ group_var }}) %>%
      stats::na.omit()

    if (ls_line == "hide") {

      plot <- ggformula::gf_point(formula, data = data, fill = fill, color = "grey30", shape = 21, size = 2) %>%
        ggformula::gf_labs(title = ifelse(base::is.null(title),
                                          base::paste0("Grouped Scatterplot of ", var1_str, " and ",
                                                       var2_str, "\nby ", group_str),
                                          title),
                           fill = base::ifelse(is.null(legend_title), group_str, legend_title),
                           ...) %>%
        finalize_plot() %>%
        ggformula::gf_theme(axis_theme) %>%
        ggformula::gf_refine(ggplot2::scale_fill_viridis_d())

      if (subtitle_display == "show") {
        plot <- plot + ggplot2::labs(subtitle = base::paste0(var1_str, " Missing: ", n_na[[1]], " | ",
                                                             var2_str, " Missing: ", n_na[[2]], " | ",
                                                             group_str, " Missing: ", group_na, "\n",
                                                             "Observations Used: ", obs_used, " | NAs Removed: Yes"))

        return(plot)
      }

    } else if (ls_line == "show") {

        # ggformula::gf_point(formula, data = data, fill = fill, color = "grey80", shape = 21, size = 2) %>%
        #   ggformula::gf_lm(color = fill) %>%
        plot <- ggformula::gf_lm(formula, data = data, color = fill) %>%
          ggformula::gf_point(size = 2) %>%
          ggformula::gf_labs(title = ifelse(base::is.null(title),
                                            base::paste0("Grouped Scatterplot of ", var1_str, " and ",
                                                         var2_str, "\nby ", group_str),
                                            title),
                             fill = base::ifelse(is.null(legend_title), group_str, legend_title),
                             ...) %>%
          finalize_plot() %>%
          ggformula::gf_theme(axis_theme) %>%
          ggformula::gf_refine(ggplot2::scale_color_viridis_d()) %>%
          ggformula::gf_point(color = "grey30", shape = 21, size = 2)

      if (subtitle_display == "show") {
        plot <- plot + ggplot2::labs(subtitle = base::paste0(var1_str, " Missing: ", n_na[[1]], " | ",
                                                             var2_str, " Missing: ", n_na[[2]], " | ",
                                                             group_str, " Missing: ", group_na, "\n",
                                                             "Observations Used: ", obs_used, " | NAs Removed: Yes"))
      }

    }



  }

}
