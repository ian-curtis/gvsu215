
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gvsu215

<!-- badges: start -->
<!-- badges: end -->

Note: Due to the fact that some functions return tables (which are
essentially HTML code), the README on GitHub will not display properly.
For best results, visit the [online version of the
README](https://gvsu215.ianacurtis.com).

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
#> Downloading GitHub repo ian-curtis/gvsu215@HEAD
#> uuid (1.1-0 -> 1.1-1) [CRAN]
#> Installing 1 packages: uuid
#> Installing package into '/private/var/folders/v9/w6xbcfv15tb191g6fft32j1h0000gn/T/RtmpCFvtWO/temp_libpath1c6a6d258a4a'
#> (as 'lib' is unspecified)
#> 
#> The downloaded binary packages are in
#>  /var/folders/v9/w6xbcfv15tb191g6fft32j1h0000gn/T//Rtmp3uHasi/downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#> * checking for file ‘/private/var/folders/v9/w6xbcfv15tb191g6fft32j1h0000gn/T/Rtmp3uHasi/remotes1fefbc282a7/ian-curtis-gvsu215-0ce1db0/DESCRIPTION’ ... OK
#> * preparing ‘gvsu215’:
#> * checking DESCRIPTION meta-information ... OK
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#> * building ‘gvsu215_0.2.0.1313.tar.gz’
#> Installing package into '/private/var/folders/v9/w6xbcfv15tb191g6fft32j1h0000gn/T/RtmpCFvtWO/temp_libpath1c6a6d258a4a'
#> (as 'lib' is unspecified)
```

## Prerequisites

In order to use this package, you will need a basic understanding of R
syntax and how to run a function in your preferred working environment.
Functions use formula syntax (e.g., `var1 ~ var2`) and thus knowledge of
formulas is needed as well.

## Examples

This package is best used for creating simple plots and charts where
extensive customization is not necessary. For instance, you may just
want to see summary statistics.

``` r
library(gvsu215)
```

``` r
num_sum(mtcars, ~wt, na_rm = TRUE)
```

Or maybe you just need percentiles:

``` r
pctile(mtcars, ~wt, probs = c(0, .25, .4, .5, .6, .75, 1))
```

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
infer_prop1(mtcars, ~vs, success = 1)

infer_paired(mtcars, var1 = ~drat, var2 = ~wt)

infer_mean2_int(mtcars, wt~vs)

suppressWarnings(infer_chisq(mtcars, cyl~gear))
suppressWarnings(infer_chisq(mtcars, cyl~gear, type = "expected"))

infer_anova(mtcars, cyl~gear, digits = 2)
```

# Help, Bug Reports, and Feature Requests

Errors happen! If you come across an error you don’t think you should
have, feel free to [create an issue on
GitHub](https://github.com/ian-curtis/gvsu215/issues). If you’d like to
suggest a change or addition, you can use the issues page for that, too!
