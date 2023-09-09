
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gvsu215

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/gvsu215)](https://CRAN.R-project.org/package=gvsu215)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Netlify Status](https://api.netlify.com/api/v1/badges/2866996e-ff21-439d-9d53-f79319b52d97/deploy-status)](https://app.netlify.com/sites/transcendent-starlight-07ae23/deploys)
<!-- badges: end -->

Note: Due to the fact that some functions return tables (which are
essentially HTML code), the README on GitHub does not display properly.
As a result, the output is an image of the resulting table rather than the actual HTML table.

`gvsu215` contains a series of wrapper functions around select
`{mosaic}` and `{ggformula}` functions, with help from `{flextable}`.
The goal is to provide introductory statistics students/learners with an
easy way to generate nice-looking plots and tables without focusing on
the code used to create them. All tables and plots generated from this
package have been doctored and have had themes pre-applied. As such,
output will not print in “messy” format to the R console and can be used
interactively, including in R Markdown documents.

This package was designed and tailored for Grand Valley State University
(course code: STA 215) but can be used by anyone.

## Installation

You can install the development version of gvsu215 from
[GitHub](https://github.com/ian-curtis/gvsu215) with:

``` r
# install.packages("devtools")
devtools::install_github("ian-curtis/gvsu215")
```

## Prerequisites

In order to use this package, you will need a basic understanding of R
syntax and how to run a function in your preferred working environment.
Functions use formula syntax (e.g., `var1 ~ var2`) and thus knowledge of
formulas is needed as well.

## Examples

This package is best used for creating simple plots and charts where
extensive customization is not necessary. For instance, you may just
want to see summary statistics, including one- or two-way tables.

``` r
library(gvsu215)
```

``` r
tbl_2var(mtcars, gear~cyl)
```

<img src="man/figures/README-tbl_two.png" width="50%" />

``` r
tbl_num_sum(mtcars, ~wt, na_rm = TRUE)
```

<img src="man/figures/README-num_sum.png" width="80%" />

Or maybe you just need percentiles:

``` r
tbl_pctile(mtcars, ~wt, probs = c(0, .25, .4, .5, .6, .75, 1))
```

<img src="man/figures/README-pctile.png" width="80%" />

There is also support for a variety of plots…

``` r
plot_bar(mtcars, ~cyl, type = "percent")
```

<img src="man/figures/README-plots-1.png" width="80%" />

``` r

plot_box(mtcars, 
         wt~gear, 
         fill = 'orangered4', 
         x = "Gear", 
         y = "Weight", 
         title = "Boxplot of Weight by Gear")
```

<img src="man/figures/README-plots-2.png" width="80%" />

``` r

plot_scatter(mtcars, wt~drat)
#> NAs always removed (in pairs) for scatterplots.
```

<img src="man/figures/README-plots-3.png" width="80%" />

… and statistical inference.

``` r
infer_1prop(mtcars, ~vs, success = 1)
```

<img src="man/figures/README-infer_1prop.png" width="80%" />

``` r
infer_paired(mtcars, var1 = ~drat, var2 = ~wt)
```

<img src="man/figures/README-infer_paired.png" width="80%" />

``` r
infer_2mean_int(mtcars, wt~vs)
```

<img src="man/figures/README-infer_2mean_int.png" width="80%" />

``` r
suppressWarnings(infer_chisq(mtcars, cyl~gear))
```

<img src="man/figures/README-infer_chisq.png" width="50%" />

``` r
suppressWarnings(infer_chisq(mtcars, cyl~gear, type = "expected"))
```

<img src="man/figures/README-infer_chisq_expected.png" width="50%" />

``` r
infer_anova(mtcars, cyl~gear, digits = 2)
```

<img src="man/figures/README-infer_anova.png" width="70%" />

# Help, Bug Reports, and Feature Requests

Errors happen! If you come across an error you don’t think you should
have, feel free to [create an issue on
GitHub](https://github.com/ian-curtis/gvsu215/issues). If you’d like to
suggest a change or addition, you can use the issues page for that, too!
