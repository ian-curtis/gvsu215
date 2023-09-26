# gvsu215 0.3.7

* Values in tables are now rounded with the standard "nearest digit" format for all numbers not between -1 and 1. Numbers between -1 and 1 are rounded using significant digits (e.g., non-zero digits after decimal) but use the same `digits` argument as the standard rounding.
* All p-value rounding has been adjusted. Any p-value less than 0.0001 will now display "< 0.0001". Other values will display the true value and follow the rounding rules mentioned above.

# gvsu215 0.3.6

* `plot_hist()`: the binwidth argument has been deprecated and is no longer available. Instead, use `breaks` which, when used, helps to line up the bins and x-axis ticks for better readability and interpretation.
* The student cheatsheet was slighlty reworked to line up with textbook chapters.

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
