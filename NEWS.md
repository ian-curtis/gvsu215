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
