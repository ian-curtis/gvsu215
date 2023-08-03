test_that("bar give message with no type given", {
  expect_message(plot_bar(mtcars, ~cyl))
})

test_that("bar works regularly with type = percent", {
  expect_no_condition(plot_bar(mtcars, ~cyl, type = "percent"))
})

test_that("bar works regularly with type = count", {
  expect_no_condition(plot_bar(mtcars, ~cyl, type = "count"))
})
