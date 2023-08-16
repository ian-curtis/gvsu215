# gvsu215 (development version)

## Breaking Changes

* Two functions have been renamed. `one_way()` is now `tbl_one()` and `two_way()` is now `tbl_two()`. The old functions are not available.

* `two_way()` (`tbl_two()`) output cleaned up
* `plot_box()` and `plot_hist()` will now not do scientific notation between certain ranges of numbers
* `corr()` now shows the observations used (since this is not always just a sum of n1 and n2 missing)
* added confidence interval to `infer_paired()`

# gvsu215 0.1.1

* general fixes to output layout and theme, especially to `two_way()`
* rounding corrections
* `plot_scatter()` now allows for a least squares line to be drawn in
* bug fix in `num_sum()`

# gvsu215 0.1.0

* All package functions are now usable
* All functions are documented
* All functions have tests
