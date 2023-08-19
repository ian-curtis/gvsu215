# infer_1prop() ####

test_that("infer prop 1 works", {
  expect_no_condition(infer_1prop(mtcars, ~vs, success = 1))
})

test_that("infer prop 1 works with digits", {
  expect_no_condition(infer_1prop(mtcars, ~vs, success = 1, digits = 4))
})

test_that("infer prop 1 works with new CL", {
  expect_no_condition(infer_1prop(mtcars, ~vs, success = 1, conf_lvl = .9))
})

test_that("infer prop 1 works with new caption", {
  expect_no_condition(infer_1prop(mtcars, ~vs, success = 1, caption = "hehe"))
})

test_that("infer prop 1 fails with wrong CL", {
  expect_error(infer_1prop(mtcars, ~vs, success = 1, conf_lvl = "90%"))
})

test_that("infer prop 1 fails with no success", {
  expect_error(infer_1prop(mtcars, ~vs))
})

test_that("infer prop 1 fails with mispelling", {
  expect_error(infer_1prop(mtcars, ~VS, success = 1))
})

# infer_2prop_int() ####

test_that("infer prop 2 interval works", {
  expect_no_condition(infer_2prop_int(mtcars, vs~am, success = 1))
})

test_that("infer prop 2 interval works with digits", {
  expect_no_condition(infer_2prop_int(mtcars, vs~am, success = 1, digits = 4))
})

test_that("infer prop 2 interval works with new CL", {
  expect_no_condition(infer_2prop_int(mtcars, vs~am, success = 1, conf_lvl = .9))
})

test_that("infer prop 2 interval works with new caption", {
  expect_no_condition(infer_2prop_int(mtcars, vs~am, success = 1, caption = "hehe"))
})

test_that("infer prop 2 interval fails with wrong CL", {
  expect_error(infer_2prop_int(mtcars, vs~am, success = 1, conf_lvl = "90%"))
})

test_that("infer prop 2 interval fails with no success", {
  expect_error(infer_2prop_int(mtcars, vs~am))
})

test_that("infer prop 2 interval fails with mispelling", {
  expect_error(infer_2prop_int(mtcars, vs~AM, success = 1))
})

# infer_2prop_test() ####

test_that("infer prop 2 test works", {
  expect_no_condition(infer_2prop_test(mtcars, vs~am, success = 1))
})

test_that("infer prop 2 test works with digits", {
  expect_no_condition(infer_2prop_test(mtcars, vs~am, success = 1, digits = 4))
})

test_that("infer prop 2 test works with new CL", {
  expect_no_condition(infer_2prop_test(mtcars, vs~am, success = 1, conf_lvl = .9))
})

test_that("infer prop 2 test works with new caption", {
  expect_no_condition(infer_2prop_test(mtcars, vs~am, success = 1, caption = "hehe"))
})

test_that("infer prop 2 test fails with wrong CL", {
  expect_error(infer_2prop_test(mtcars, vs~am, success = 1, conf_lvl = "90%"))
})

test_that("infer prop 2 test fails with no success", {
  expect_error(infer_2prop_test(mtcars, vs~am))
})

test_that("infer prop 2 test fails with mispelling", {
  expect_error(infer_2prop_test(mtcars, vs~AM, success = 1))
})

# infer_1mean ####

test_that("infer mean 1 works", {
  expect_no_condition(infer_1mean(mtcars, ~wt))
})

test_that("infer mean 1 works with digits", {
  expect_no_condition(infer_1mean(mtcars, ~wt, digits = 4))
})

test_that("infer mean 1 works with new CL", {
  expect_no_condition(infer_1mean(mtcars, ~wt, conf_lvl = .9))
})

test_that("infer mean 1 works with new caption", {
  expect_no_condition(infer_1mean(mtcars, ~wt, caption = "hehe"))
})

test_that("infer mean 1 fails with wrong CL", {
  expect_error(infer_1mean(mtcars, ~wt, conf_lvl = "90%"))
})

test_that("infer mean 1 fails with mispelling", {
  expect_error(infer_1mean(mtcars, ~Wt))
})

# infer_paired() ####

test_that("paired works", {
  expect_no_condition(infer_paired(mtcars, var1 = ~drat, var2 = ~wt))
})

test_that("paired works with digits", {
  expect_no_condition(infer_paired(mtcars, var1 = ~drat, var2 = ~wt, digits = 4))
})

test_that("paired works with new CL", {
  expect_no_condition(infer_paired(mtcars, var1 = ~drat, var2 = ~wt, conf_lvl = 0.9))
})

test_that("paired works with new caption", {
  expect_no_condition(infer_paired(mtcars, var1 = ~drat, var2 = ~wt, caption = "hehe"))
})

test_that("paired fails with wrong CL", {
  expect_error(infer_paired(mtcars, var1 = ~drat, var2 = ~wt, conf_lvl = "95%"))
})

test_that("paired fails with mispelling", {
  expect_error(infer_paired(mtcars, var1 = ~drat, var2 = ~Wt))
})

# infer_2mean_int() ####

test_that("infer mean 2 works", {
  expect_no_condition(infer_2mean_int(mtcars, wt~vs))
})

test_that("infer mean 2 works with digits", {
  expect_no_condition(infer_2mean_int(mtcars, wt~vs, digits = 4))
})

test_that("infer mean 2 works with new CL", {
  expect_no_condition(infer_2mean_int(mtcars, wt~vs, conf_lvl = .9))
})

test_that("infer mean 2 works with new caption", {
  expect_no_condition(infer_2mean_int(mtcars, wt~vs, caption = "hehe"))
})

test_that("infer mean 2 fails with wrong CL", {
  expect_error(infer_2mean_int(mtcars, wt~vs, conf_lvl = "90%"))
})

test_that("infer mean 2 fails with mispelling", {
  expect_error(infer_2mean_int(mtcars, Wt~vs))
})

test_that("infer mean 2 fails with wrong order", {
  expect_error(infer_2mean_int(mtcars, vs~wt))
})

test_that("infer mean 2 fails with too many levels", {
  expect_error(infer_2mean_int(mtcars, wt~cyl))
})


# infer_2mean_test() ####

test_that("infer mean 2 test works", {
  expect_no_condition(infer_2mean_test(mtcars, wt~vs))
})

test_that("infer mean 2 test works with digits", {
  expect_no_condition(infer_2mean_test(mtcars, wt~vs, digits = 4))
})

test_that("infer mean 2 test works with new CL", {
  expect_no_condition(infer_2mean_test(mtcars, wt~vs, conf_lvl = .9))
})

test_that("infer mean 2 test works with new caption", {
  expect_no_condition(infer_2mean_test(mtcars, wt~vs, caption = "hehe"))
})

test_that("infer mean 2 test fails with wrong CL", {
  expect_error(infer_2mean_test(mtcars, wt~vs, conf_lvl = "90%"))
})

test_that("infer mean 2 test fails with mispelling", {
  expect_error(infer_2mean_test(mtcars, Wt~vs))
})

test_that("infer mean 2 test fails with wrong order", {
  expect_error(infer_2mean_test(mtcars, vs~wt))
})

test_that("infer mean 2 test fails with too many levels", {
  expect_error(infer_2mean_test(mtcars, wt~cyl))
})
