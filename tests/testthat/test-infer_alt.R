test_that("infer prop 2 works", {
  expect_no_condition(infer_2prop(mtcars, vs~am, success = 1))
})

test_that("infer prop 2 works with digits", {
  expect_no_condition(infer_2prop(mtcars, vs~am, success = 1, digits = 4))
})

test_that("infer prop 2 works with new CL", {
  expect_no_condition(infer_2prop(mtcars, vs~am, success = 1, conf_lvl = .9))
})

test_that("infer prop 2 works with new caption", {
  expect_no_condition(infer_2prop(mtcars, vs~am, success = 1, caption = "hehe"))
})

test_that("infer prop 2 fails with wrong CL", {
  expect_error(infer_2prop(mtcars, vs~am, success = 1, conf_lvl = "90%"))
})

test_that("infer prop 2 fails with no success", {
  expect_error(infer_2prop(mtcars, vs~am))
})

test_that("infer prop 2 fails with mispelling", {
  expect_error(infer_2prop(mtcars, vs~AM, success = 1))
})

test_that("infer prop 2 works with conf int", {
  expect_no_condition(infer_2prop(mtcars, vs~am, success = 1, conf_int = "show"))
})

# mean 2
test_that("infer mean 2 works", {
  expect_no_condition(infer_2mean(mtcars, wt~vs))
})

test_that("infer mean 2 works with digits", {
  expect_no_condition(infer_2mean(mtcars, wt~vs, digits = 4))
})

test_that("infer mean 2 works with new CL", {
  expect_no_condition(infer_2mean(mtcars, wt~vs, conf_lvl = .9))
})

test_that("infer mean 2 works with new caption", {
  expect_no_condition(infer_2mean(mtcars, wt~vs, caption = "hehe"))
})

test_that("infer mean 2 fails with wrong CL", {
  expect_error(infer_2mean(mtcars, wt~vs, conf_lvl = "90%"))
})

test_that("infer mean 2 fails with mispelling", {
  expect_error(infer_2mean(mtcars, Wt~vs))
})

test_that("infer mean 2 fails with wrong order", {
  expect_error(infer_2mean(mtcars, vs~wt))
})

test_that("infer mean 2 fails with too many levels", {
  expect_error(infer_2mean(mtcars, wt~cyl))
})

test_that("infer prop 2 works with conf_int", {
  expect_no_condition(infer_2mean(mtcars, wt~vs, conf_int = "show"))
})
