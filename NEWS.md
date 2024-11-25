# gvsu215 0.6.1

* Adjusted captions in `infer_mean_*` and `infer_prop_*`. Some intervals were not showing confidence levels and some tests were. These should be back in their regular places.
* `infer_2mean_test()` now has a `mu0` argument to be used with the difference between the means is not equal to 0.

# gvsu215 0.6.0

* `plot_bar()` now includes the ability to specify a stacked bar chart. `fill` must be a variable, then use the `layout` argument set to `stack`. (`sbs`, side-by-side, is the default).
* Various small fixes to documentation and vignettes

# gvsu215 0.5.5

* All functions that have an `na_rm` argument now default to `TRUE` (rather than `FALSE`)
* KNOWN ISSUE: Table captions in pdf output have wonky spacing. This is believed to be a bug within `flextable` and is in progress.

# gvsu215 0.5.4

* Updated spacing in tables for Word output
  * Line breaks in table headers are now forced with `\n`, users will need to specify their own line breaks in variable names
  * Max width of tables for non-interactive and non-HTML output (e.g., PDF and Word) is 6.9 inches. No max width is given for other outputs.
  * Note added to extending default behavior article.
* The spacing between clusters of the grouped bar chart (`plot_bar()` used with the `fill` argument set to a variable) has been increased. This was done by decreasing the width of the actual bars so having more categories in the x-axis variable might cause this spacing to decrease.
  

# gvsu215 0.5.3

* Revert / clarify that all p-values for tests on the mean and the proportion are two-tailed

# gvsu215 0.5.2

* Various calculation corrections. Mostly in functions dealing with proportion inference regarding standard error and confidence intervals.
* P-values now show as one-tailed

# gvsu215 0.5.1

* One-sample inference functions have a new argument. These can be used to specify the null hypothesis value you are testing against. The `null` argument no longer works.
  * `mu0` for means
  * `p0` for proportions

# gvsu215 0.5.0

* The axes for `tbl_2var()` were flipped so I flipped them back the right way (which is the way it was before).
* All inference functions that deal with means have a new argument: `null` which, by default, is set to zero. Use this argument
* Corrections to cheatsheet and extending behavior articles.

# gvsu215 0.4.7

* You can now see proportions in `tbl_1var()` in addition to percents. Use `with_prop = "yes"`.
* Ever wanted to make missing values disappear in one-way and two-way tables? Now you can! `tbl_1var()` and `tbl_2var()` support the `na_rm` argument.
* The axes for `tbl_2var()` were flipped so I flipped them back the right way.
* NEW FUNCTIONS / RENAMED FUNCTIONS which allow for hypothesis tests for one sample proportion and one sample mean (previously we only had support for confidence intervals)
  * `infer_1prop()` is now `infer_1prop_int()`
  * `infer_1mean()` is now `infer_1mean_int()`
  * NEW: `infer_1prop_test()`
  * NEW: `infer_1mean_test()`
* `plot_box()` now has a `breaks` argument (which works just like that of `plot_hist()`). The plot should also be a little smarter by adding some extra padding around the min and the max of the plot.

# gvsu215 0.4.6

* A new argument was given to `plot_bar()`: `orient`, which can take on either "vertical" or "horizontal" and can be used to flip the axes of the plot.
* `plot_bar()` and `plot_box()` have a new argument: `dodge` which will take on an integer greater than 0 (default 1) to dodge x-axis labels should they overlap.
* `plot_hist()` will now work again with groups. Oops!

# gvsu215 0.4.5

* The student cheatsheet was updated. Functions that don't have an `na_rm` argument no longer show that as an option.
* The Instructor's Guide was renamed to accurately describe it's content.
* Histograms now default to showing non-overlapping x-axis values, if necessary.
* `infer_chisq` now removes all NAs automatically. Output now shows how many observations there were originally as well as how many were used for the output. 

# gvsu215 0.4.4

* `tbl_num_sum()` output: If missing values are removed (i.e., `na_rm = TRUE`) the number missing values are still shown in the table. Previously this column was removed but it still is good information to show.

# gvsu215 0.4.3

* Appearance changes for some tables (such as font size and borders)
* Correct error from previous release where the function `dplyr::na_if()` was used without the `::` syntax.

# gvsu215 0.4.2

* You now can use `base::read.csv()` with the functions! A unique case presented itself where a missing value was recorded as "" (the empty string) and was not detected by default from ``read.csv()`. All functions now search character variables for empty strings and replace them with true NAs. This could be expanded in the future.

# gvsu215 0.4.1

* All tables should be properly right-aligned
* Cosmetic changes in multiple plots and tables
* Add padding zeroes in the case where the rounded number is shorter than the number of digits supplied
* Clarification updates to captions/titles in `plot_bar` and `tbl_pctile`

# gvsu215 0.4.0

* All arguments the previously took either TRUE or FALSE as a value have been converted into vector-style arguments. For example in `plot_scatter()`, the `ls_line` argument now accepts either "show" or "hide". The only exception to this is `na_rm`, wherever it appears.
* Error and warning messages were updated. They are now slightly more user-friendly thanks to the `cli` and `rlang` packages.
* More comments were added to the source code to help me remember what I was doing six months from now.

# gvsu215 0.3.8

* Corrected error in `infer_paired()` which had an incorrect use of the `::` syntax
* Updated figures displayed in README (to now accurately show the right alignment of table body cells)

# gvsu215 0.3.7

* Values in tables are now rounded with the standard "nearest digit" format for all numbers not between -1 and 1. Numbers between -1 and 1 are rounded using significant digits (i.e., non-zero digits after decimal) but use the same `digits` argument as the standard rounding.
* All p-value rounding has been adjusted. Any p-value less than 0.0001 will now display "< 0.0001". Other values will display the true value and follow the rounding rules mentioned above.

# gvsu215 0.3.6

* `plot_hist()`: the binwidth argument has been deprecated and is no longer available. Instead, use `breaks` which, when used, helps to line up the bins and x-axis ticks for better readability and interpretation.
* The student cheatsheet was slightly reworked to line up with textbook chapters.

# gvsu215 0.3.5

* `tbl_num_sum()` now shows number of NAs in caption when `na_rm` is TRUE and does not show the "missing" column (which was all 0s in this case). This funciont will also now correctly display "NAs Removed" in the caption.
* Default table output width is now 6 inches to avoid bleeding into margins of PDF and Word documents.

# gvsu215 0.3.4

* Package now depends on `ggformula` (rather than imports) to help avoid a potential namespace issue when users have not loaded `ggformula`.
* Small adjustments to plot themes
* The output from `infer_chisq(., type = "expected")` now shows both variable names on the table.

# gvsu215 0.3.3

* Mostly internal changes
  * Small documentation updates
  * Reworked in-function variable names
  * Updated internal tests
  * Redesigned student cheatsheet, built instructor's guide

# gvsu215 0.3.2

* Updated documentation (including how to add a least squares line to `plot_scatter()`).
* Tried to change a legend title on a grouped scatterplot and it didn't work? Now it does!
* A new vignette has appeared! It contains a code shell/template for each of the functions in the package. For out-of-the-box use, simply copy the code and fill in the blanks.
* Some error messages will show up less frequently.

# gvsu215 0.3.1

## Updates

* `plot_bar()` will now show colors correctly when `type = "count"` is specified.
* `tbl_num_sum()` will no longer show NAs when no NAs are present and `na_rm = FALSE` is given.
* The second variable entered in `infer_anova()` is now coerced into a factor so that degrees of freedom are correctly calculated.

## New Functions

You now may use `infer_2mean()` and `infer_2prop()` to perform a two-sample proportion or mean test, respectively. The main perk to this is the `conf_int` argument. When specified to TRUE, a confidence interval will be given in addition to the hypothesis test output.

These functions are provided as an alternative to `infer_2prop_int()`, `infer_2prop_test()`, `infer_2mean_int()`, and `infer_2mean_test()`. Rather than having two functions for intervals and tests, you can now do it in one function if you so choose.

# gvsu215 0.3.0

## Major Changes

* Many functions have changed names (the old names are not available) to maintain consistency and ease of typing the function names. These functions are:
  * `tbl_one()` --> `tbl_1var()`
  * `tbl_two()` --> `tbl_2var()`
  * `num_sum()` --> `tbl_num_sum()`
  * `pctile()` --> `tbl_pctile()`
  * `corr()` --> `tbl_corr()`
  * `infer_mean1()` --> `infer_1mean()`
  * `infer_mean2()` --> `infer_2mean()`
  * `infer_prop1()` --> `infer_1prop()`
  * `infer_mean2_int()` --> `infer_2mean_int()`
  * `infer_mean2_test()` --> `infer_2mean_test()`
  * `infer_prop2_int()` --> `infer_2prop_int()`
  * `infer_prop2_test()` --> `infer_2prop_test()`

## Other Changes

* The package vignette (README) has been changed to import images of all tables. The HTML output of the code does not work well with GitHub and the tables don't appear there.

# gvsu215 0.2.0

## Major Changes

* Two functions have been renamed. `one_way()` is now `tbl_one()` and `two_way()` is now `tbl_two()`. The old functions are not available.
* `plot_scatter()`: The default value for `legend_title` is now NULL (which is then supplied with the variable name). You can still supply a new string for the legend title.

## Other Changes

* `two_way()` (`tbl_two()`) output cleaned up
* `plot_box()` and `plot_hist()` will now not do scientific notation between certain ranges of numbers
* `corr()` now shows the observations used (since this is not always just a sum of n1 and n2 missing)
* added confidence interval to `infer_paired()`
* The two-sample family of functions now show the correct levels of the grouping variable and the respective calculations about them.

# gvsu215 0.1.1

* general fixes to output layout and theme, especially to `two_way()`
* rounding corrections
* `plot_scatter()` now allows for a least squares line to be drawn in
* bug fix in `num_sum()`

# gvsu215 0.1.0

* All package functions are now usable
* All functions are documented
* All functions have tests
