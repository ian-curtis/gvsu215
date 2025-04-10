# plot_bar() ####

test_that("bar gives message with no type given", {
  expect_no_error(plot_bar(mtcars, ~cyl))
})

test_that("bar works regularly with type = percent", {
  expect_no_condition(plot_bar(mtcars, ~cyl, type = "percent"))
})

test_that("bar works with type = count", {
  expect_no_condition(plot_bar(mtcars, ~cyl, type = "count"))
})

test_that("bar works with removing NAs", {
  expect_no_condition(plot_bar(mtcars, ~cyl, type = "percent", na_rm = TRUE))
})

test_that("bar works with new fill color", {
  expect_no_condition(plot_bar(mtcars, ~cyl, type = "percent", fill = "yellowgreen"))
})

test_that("bar fails with neither percent nor count", {
  expect_error(plot_bar(mtcars, ~cyl, type = "idk"))
})

test_that("bar fails with mispelling", {
  expect_error(plot_bar(mtcars, ~Cyl, type = "count"))
})

test_that("bar fails with long formula", {
  expect_error(plot_bar(mtcars, cyl~gear, type = "count"))
})

test_that("bar fails with mispelling data", {
  expect_error(plot_bar(MTCARS, ~Cyl, type = "count"))
})

test_that("group bar gives message with no type given", {
  expect_no_error(plot_bar(mtcars, ~cyl, fill = ~gear))
})

test_that("group bar works with removing NAs", {
  expect_no_condition(plot_bar(mtcars, ~cyl, fill = ~gear, type = "percent", na_rm = TRUE))
})

test_that("group bar works with count", {
  expect_no_condition(plot_bar(mtcars, ~cyl, type = "count", fill = ~gear))
})

test_that("bar works with change title", {
  expect_no_condition(plot_bar(mtcars, ~cyl, title = "hehe", type = "percent"))
})

test_that("bar works with label change", {
  expect_no_warning(plot_box(mtcars, wt~gear, x = "yay", y = "yay"))
})

test_that("bar works with group fill and stack position", {
  expect_no_warning(plot_bar(mtcars, ~cyl, fill = ~gear, layout = "stack"))
})

test_that("bar warns with variable fill and stack position", {
  expect_error(plot_bar(mtcars, ~cyl, fill = "red", layout = "stack"))
})




# plot_box() ####

test_that("boxplot works normally", {
  expect_no_condition(plot_box(mtcars, ~wt))
})

test_that("boxplot works with removing NAs", {
  expect_no_condition(plot_box(mtcars, ~wt, na_rm = TRUE))
})

test_that("boxplot works with title changed", {
  expect_no_condition(plot_box(mtcars, ~wt, title = "hehe"))
})

test_that("boxplot works normally with new fill color", {
  expect_no_condition(plot_box(mtcars, wt~gear, fill = 'orangered4'))
})

test_that("boxplot works normally with label changes", {
  expect_no_condition(plot_box(mtcars, wt~gear, x = "yay", y = "hehe"))
})

test_that("group boxplot works normally", {
  expect_no_condition(plot_box(mtcars, wt~gear))
})

test_that("group boxplot works normally with NAs removed", {
  expect_no_condition(plot_box(mtcars, wt~gear, na_rm = TRUE))
})

test_that("group boxplot works normally with title changed", {
  expect_no_condition(plot_box(mtcars, wt~gear, title = "hehe"))
})

test_that("group boxplot fails with mispelling", {
  expect_error(plot_box(mtcars, Wt~gear))
})

test_that("boxplot fails with mispelling", {
  expect_error(plot_box(Mtcars, wt~gear))
})

test_that("boxplot works with swapping order", {
  expect_no_condition(plot_box(mtcars, gear~wt))
})

# plot_hist() ####

test_that("histogram works", {
  expect_no_warning(plot_hist(mtcars, ~drat, breaks = seq(3, 6, 0.5)))
})

test_that("histogram warns with no breaks", {
  expect_condition(plot_hist(mtcars, ~drat))
})

test_that("histogram works with new fill", {
  expect_no_condition(plot_hist(mtcars, ~drat, breaks = seq(3, 6, 0.5), fill = "#183590"))
})

test_that("histogram works with NAs removed", {
  expect_no_condition(plot_hist(mtcars, ~drat, breaks = seq(3, 6, 0.5), na_rm = TRUE))
})

test_that("histogram works with new title", {
  expect_no_condition(plot_hist(mtcars, ~drat, breaks = seq(3, 6, 0.5), title = "hehe"))
})

test_that("histogram works with new axis labels", {
  expect_no_condition(plot_hist(mtcars, ~drat, breaks = seq(3, 6, 0.5), x = "test"))
})

test_that("histogram fails with mispelling", {
  expect_error(plot_hist(mtcars, ~Drat, breaks = seq(3, 6, 0.5)))
})

test_that("group histogram works", {
  expect_condition(plot_hist(mtcars, ~drat, binwidth = 0.05, group = ~cyl))
})

test_that("histogram works with different columns", {
  expect_no_condition(plot_hist(mtcars, ~drat, breaks = seq(2, 5, 0.25), group = ~cyl, group_cols = 2))
})

test_that("histogram fails with many vars in formula", {
  expect_error(plot_hist(mtcars, drat~cyl, breaks = seq(2, 5, 0.25)))
})

test_that("grouped histogram fails with mispelling", {
  expect_error(plot_hist(Mtcars, ~drat, breaks = seq(2, 5, 0.25)))
})


# plot_scatter() ####

test_that("scatter works", {
  expect_no_error(plot_scatter(mtcars, wt~drat))
})

test_that("scatter works with new fill color", {
  expect_no_error(plot_scatter(mtcars, wt~drat, fill = "#49208a"))
})

test_that("scatter works with new title", {
  expect_no_error(plot_scatter(mtcars, wt~drat, title = "a scatterplot!"))
})

test_that("scatter works with new labels", {
  expect_no_error(plot_scatter(mtcars, wt~drat, x = "yaya", y = "ayay"))
})

test_that("scatter works with both lines", {
  expect_no_error(plot_scatter(mtcars, wt~drat, axis_lines = "both"))
})

test_that("group scatter works", {
  expect_no_error(plot_scatter(mtcars, wt~drat, fill = ~cyl))
})

test_that("scatter works with new legend title", {
  expect_no_error(plot_scatter(mtcars, wt~drat, fill = ~cyl, legend_title = "Cyl"))
})

test_that("scatter warns with fill string and legend title", {
  expect_message(plot_scatter(mtcars, wt~drat, legend_title = "Hey!"))
})

test_that("group scatter works with axis lines", {
  expect_no_error(plot_scatter(mtcars, wt~drat, fill = ~cyl, axis_lines = "both"))
})

test_that("group scatter fails with mispelling", {
  expect_error(plot_scatter(mtcars, wt~Drat, fill = ~cyl, axis_lines = "both"))
})

test_that("scatter works with least squares line", {
  expect_no_error(plot_scatter(mtcars, wt~drat, fill = ~cyl, ls_line = "show"))
})

test_that("group scatter works with least squares line", {
  expect_no_error(plot_scatter(mtcars, wt~drat, fill = ~cyl, ls_line = "show"))
})





