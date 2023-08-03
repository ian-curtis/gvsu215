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
      
      
      lvls <- dplyr::pull(data, var)
      big_lvl <- base::max(base::nchar(unique(lvls)), na.rm = TRUE)
      
      if (big_lvl > 20) {
        
        data <- data %>% 
          dplyr::mutate("{var}" := gsub(" ", "\n", {{ var }}))
        
      }
      
      data %>% 
        # dplyr::mutate("{var}" := gsub(" ", "\n", {{ var }})) %>% 
        ggformula::gf_percents(formula, fill = fill) %>% 
        ggformula::gf_refine(scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)), 
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
        dplyr::mutate("{var}" := base::gsub(" ", "\n", {{ var }})) %>%
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

plot_box <- function(data, formula, fill = "grey80", title = NULL, na_rm = FALSE, ...) {
  
  # error catching
  check_test(ggformula::gf_boxplot(formula, data = data, geom = "errorbar", linewidth = 2, width = 0))
  
  # code
  var <- formula[[2]]
  str_of_var <- base::deparse(base::substitute(var))
  
  if (length(formula) == 2) {
    
    na <- find_na(data, formula)
    
    if (na_rm == TRUE) {
      
      data <- data %>% 
        dplyr::select(var) %>% 
        stats::na.omit()
      
    }
    
    ggformula::gf_boxplot(formula, data = data, geom = "errorbar", linewidth = 2, width = 0) %>% 
      ggformula::gf_boxplot(formula, data = data, fill = fill, width = 0.5, lwd = 1, color = "black", 
                            outlier.shape = 21, outlier.size = 2.5, outlier.color = "grey70", 
                            outlier.fill = "black",notchwidth = 2) %>% 
      ggformula::gf_labs(x = str_of_var, 
              title = base::ifelse(base::is.null(title), 
                                   base::paste("Boxplot of", str_of_var), 
                                   title),
              subtitle = base::paste("Missing:", na, "|",
                                     "NAs Removed:", base::ifelse(na_rm == FALSE, "No", "Yes")),
              ...) %>% 
      finalize_plot() %>% 
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
    
    ggformula::gf_boxplot(formula, data = data, geom = "errorbar", linewidth = 2, width = 0) %>% 
      ggformula::gf_boxplot(formula, data = data, fill = fill, width = 0.5, lwd = 1, color = "black", 
                            outlier.shape = 21, outlier.size = 2.5, outlier.color = "grey70", 
                            outlier.fill = "black",notchwidth = 2) %>% 
      ggformula::gf_labs(title = ifelse(base::is.null(title), 
                             paste("Boxplot of", str_of_var, "by", str_of_by), 
                             title), 
                         subtitle = paste(str_of_var, "Missing:", na[[1]], "|",
                                          str_of_by, "Missing:", na[[2]], "|",
                                          "NAs Removed:", base::ifelse(na_rm == FALSE, "No", "Yes")),
                         ...) %>% 
      finalize_plot() %>% 
      ggformula::gf_theme(axis.text.x = ggplot2::element_text(size = 15),
               axis.text.y = ggplot2::element_text(size = 15))
    
  }
  
}


plot_hist <- function(data, formula, fill = "#0032A0", breaks = NULL, facet = NULL, facet_cols = NULL, title = NULL, na_rm = FALSE, ...) {
  
  # error catching
  if (is.null(breaks)) {
    
    warning("No value for breaks supplied. Your histogram may not show your data accurately.")
    
  }
  
  check_test(ggformula::gf_histogram(formula, data = data, type="count", breaks = breaks, fill = fill, color = 'grey40', alpha = 100))
  
  
  # code
  var <- formula[[2]]
  str_of_var <- base::deparse(base::substitute(var))
  
  
  if (base::is.null(facet)) {
    
    n_na <- find_na(data, formula)
    
    if (na_rm == TRUE) {
      
      data <- data %>% 
        dplyr::select({{ var }}) %>% 
        stats::na.omit()
    }
    
    ggformula::gf_histogram(formula, data = data, type="count", breaks = breaks, fill = fill, color = 'grey40', alpha = 100) %>% 
      ggformula::gf_refine(ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)))) %>% 
      ggformula::gf_labs(y = "Count",
                         title = base::ifelse(base::is.null(title), 
                                        base::paste("Histogram of", str_of_var), 
                                        title),
                         subtitle = base::paste("Missing:", n_na, "|", "NAs Removed:", 
                                                base::ifelse(na_rm == FALSE, "No", "Yes")),
                         ...) %>% 
      finalize_plot()
  } else {
    
    facet_var <- facet[[2]]
    str_of_facet <- base::deparse(base::substitute(facet_var))
    
    var_na <- find_na(data, formula)
    facet_na <- find_na(data, facet)
    
    if (na_rm == TRUE) {
      data <- data %>% 
        dplyr::select({{ var }}, {{ facet_var }}) %>% 
        stats::na.omit()
    }
    
    ggformula::gf_histogram(formula, data = data, breaks = breaks, fill = fill, color = "grey40", alpha = 100) %>% 
      ggformula::gf_facet_wrap(facet, ncol = ifelse(is.null(facet_cols), 1, facet_cols)) %>% 
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
      ggformula::gf_theme(panel.border = ggplot2::element_rect(color = "black", fill = NA))
    
  }
  
}


plot_scatter <- function(data, formula, fill = "#0032a0", title = NULL, legend_title = "Legend", axis_lines = c("none", "both"), ...) {
  
  # error catching
  message("NAs always removed for scatterplots.")
  
  check_test(ggformula::gf_point(formula, data = data, fill = fill, color = "grey80", shape = 21, size = 2))
  
  # code
  ind_var <- formula[[2]]
  ind_str <- base::deparse(base::substitute(ind_var))
  
  dep_var <- formula[[3]]
  dep_str <- base::deparse(base::substitute(dep_var))
  
  n_na <- find_na(data, formula, n = 2)
  
  axis_lines <- base::match.arg(axis_lines)
  
  if (axis_lines == "none") {
    
    axis_theme <- ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                                 panel.grid.major.x = ggplot2::element_blank())
    
  } else {
    
    axis_theme <- ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = 'grey70', linewidth = .5),
                                 panel.grid.major.x = ggplot2::element_line(color = 'grey70', linewidth = .5))
    
  }
  
  
  if (base::is.character(fill)) {
      
    ggformula::gf_point(formula, data = data, fill = fill, color = "grey80", shape = 21, size = 2) %>% 
      ggformula::gf_labs(title = base::ifelse(base::is.null(title), 
                                              base::paste("Scatterplot of", ind_str, "by", dep_str), 
                                              title), 
                         subtitle = base::paste(ind_str, "Missing:", n_na[[1]], "|",
                                                dep_str, "Missing:", n_na[[2]], "|",
                                                "NAs Removed: Yes"),
                         ...) %>% 
      finalize_plot() %>% 
      ggformula::gf_theme(axis_theme)
    
    
    
  } else {
    
    group_var <- fill[[2]]
    group_str <- base::deparse(base::substitute(group_var))
    
    group_na <- find_na(data, fill)
    
    fctr <- data %>% 
      dplyr::select({{ group_var }}) %>% 
      is.factor()
    
    if (fctr == FALSE) {
      
      data <- data %>% 
        dplyr::mutate("{group_str}" := base::as.factor({{ group_var }}))
      
    }
    
    
    
    data <- data %>%
      dplyr::select({{ ind_var }}, {{ dep_var }}, {{ group_var }}) %>% 
      stats::na.omit()
    
    ggformula::gf_point(formula, data = data, fill = fill, color = "grey80", shape = 21, size = 2) %>% 
      ggformula::gf_labs(title = ifelse(base::is.null(title), 
                                        base::paste0("Grouped Scatterplot of ", ind_str, " and ", 
                                                     dep_str, "\n by ", group_str), 
                                        title), 
                         fill = legend_title, 
                         subtitle = base::paste0(ind_str, " Missing: ", n_na[[1]], " | ",
                                                dep_str, " Missing: ", n_na[[2]], "\n",
                                                group_str, " Missing: ", group_na, " | ",
                                                "NAs Removed: Yes"),
                         ...) %>% 
      finalize_plot() %>% 
      ggformula::gf_theme(axis_theme)
    
    
    
  }
  
}
