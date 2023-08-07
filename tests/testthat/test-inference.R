# infer_prop1() ####

test_that("infer prop 1 works", {
  expect_no_condition(infer_prop1(mtcars, ~vs, success = 1))
})

test_that("infer prop 1 works with digits", {
  expect_no_condition(infer_prop1(mtcars, ~vs, success = 1, digits = 4))
})

test_that("infer prop 1 works with new CL", {
  expect_no_condition(infer_prop1(mtcars, ~vs, success = 1, conf_lvl = .9))
})

test_that("infer prop 1 works with new caption", {
  expect_no_condition(infer_prop1(mtcars, ~vs, success = 1, caption = "hehe"))
})

test_that("infer prop 1 fails with wrong CL", {
  expect_error(infer_prop1(mtcars, ~vs, success = 1, conf_lvl = "90%"))
})

test_that("infer prop 1 fails with no success", {
  expect_error(infer_prop1(mtcars, ~vs))
})

test_that("infer prop 1 fails with mispelling", {
  expect_error(infer_prop1(mtcars, ~VS, success = 1))
})

# infer_prop2_int() ####

test_that("infer prop 2 interval works", {
  expect_no_condition(infer_prop2_int(mtcars, vs~am, success = 1))
})

test_that("infer prop 2 interval works with digits", {
  expect_no_condition(infer_prop2_int(mtcars, vs~am, success = 1, digits = 4))
})

test_that("infer prop 2 interval works with new CL", {
  expect_no_condition(infer_prop2_int(mtcars, vs~am, success = 1, conf_lvl = .9))
})

test_that("infer prop 2 interval works with new caption", {
  expect_no_condition(infer_prop2_int(mtcars, vs~am, success = 1, caption = "hehe"))
})

test_that("infer prop 2 interval fails with wrong CL", {
  expect_error(infer_prop2_int(mtcars, vs~am, success = 1, conf_lvl = "90%"))
})

test_that("infer prop 2 interval fails with no success", {
  expect_error(infer_prop2_int(mtcars, vs~am))
})

test_that("infer prop 2 interval fails with mispelling", {
  expect_error(infer_prop2_int(mtcars, vs~AM, success = 1))
})

# infer_prop2_test() ####

test_that("infer prop 2 test works", {
  expect_no_condition(infer_prop2_test(mtcars, vs~am, success = 1))
})

test_that("infer prop 2 test works with digits", {
  expect_no_condition(infer_prop2_test(mtcars, vs~am, success = 1, digits = 4))
})

test_that("infer prop 2 test works with new CL", {
  expect_no_condition(infer_prop2_test(mtcars, vs~am, success = 1, conf_lvl = .9))
})

test_that("infer prop 2 test works with new caption", {
  expect_no_condition(infer_prop2_test(mtcars, vs~am, success = 1, caption = "hehe"))
})

test_that("infer prop 2 test fails with wrong CL", {
  expect_error(infer_prop2_test(mtcars, vs~am, success = 1, conf_lvl = "90%"))
})

test_that("infer prop 2 test fails with no success", {
  expect_error(infer_prop2_test(mtcars, vs~am))
})

test_that("infer prop 2 test fails with mispelling", {
  expect_error(infer_prop2_test(mtcars, vs~AM, success = 1))
})

# infer_mean1 ####

test_that("infer mean 1 works", {
  expect_no_condition(infer_mean1(mtcars, ~wt))
})

test_that("infer mean 1 works with digits", {
  expect_no_condition(infer_mean1(mtcars, ~wt, digits = 4))
})

test_that("infer mean 1 works with new CL", {
  expect_no_condition(infer_mean1(mtcars, ~wt, conf_lvl = .9))
})

test_that("infer mean 1 works with new caption", {
  expect_no_condition(infer_mean1(mtcars, ~wt, caption = "hehe"))
})

test_that("infer mean 1 fails with wrong CL", {
  expect_error(infer_mean1(mtcars, ~wt, conf_lvl = "90%"))
})

test_that("infer mean 1 fails with mispelling", {
  expect_error(infer_mean1(mtcars, ~Wt))
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

# infer_mean2_int() ####

test_that("infer mean 2 works", {
  expect_no_condition(infer_mean2_int(mtcars, wt~vs))
})

test_that("infer mean 2 works with digits", {
  expect_no_condition(infer_mean2_int(mtcars, wt~vs, digits = 4))
})

test_that("infer mean 2 works with new CL", {
  expect_no_condition(infer_mean2_int(mtcars, wt~vs, conf_lvl = .9))
})

test_that("infer mean 2 works with new caption", {
  expect_no_condition(infer_mean2_int(mtcars, wt~vs, caption = "hehe"))
})

test_that("infer mean 2 fails with wrong CL", {
  expect_error(infer_mean2_int(mtcars, wt~vs, conf_lvl = "90%"))
})

test_that("infer mean 2 fails with mispelling", {
  expect_error(infer_mean2_int(mtcars, Wt~vs))
})

test_that("infer mean 2 fails with wrong order", {
  expect_error(infer_mean2_int(mtcars, vs~wt))
})

test_that("infer mean 2 fails with too many levels", {
  expect_error(infer_mean2_int(mtcars, wt~cyl))
})


# infer_mean2_test() ####

test_that("infer mean 2 works", {
  expect_no_condition(infer_mean2_test(mtcars, wt~vs))
})

test_that("infer mean 2 works with digits", {
  expect_no_condition(infer_mean2_test(mtcars, wt~vs, digits = 4))
})

test_that("infer mean 2 works with new CL", {
  expect_no_condition(infer_mean2_test(mtcars, wt~vs, conf_lvl = .9))
})

test_that("infer mean 2 works with new caption", {
  expect_no_condition(infer_mean2_test(mtcars, wt~vs, caption = "hehe"))
})

test_that("infer mean 2 fails with wrong CL", {
  expect_error(infer_mean2_test(mtcars, wt~vs, conf_lvl = "90%"))
})

test_that("infer mean 2 fails with mispelling", {
  expect_error(infer_mean2_test(mtcars, Wt~vs))
})

test_that("infer mean 2 fails with wrong order", {
  expect_error(infer_mean2_test(mtcars, vs~wt))
})

test_that("infer mean 2 fails with too many levels", {
  expect_error(infer_mean2_test(mtcars, wt~cyl))
})


# infer_reg() ####

test_that("regression works", {
  expect_no_condition(infer_reg(mtcars, drat~wt))
})

test_that("regression works with new caption", {
  expect_no_condition(infer_reg(mtcars, drat~wt, caption = "hehe"))
})

test_that("regression works with digits", {
  expect_no_condition(infer_reg(mtcars, drat~wt, digits = 4))
})

test_that("regression works with a different order", {
  expect_no_condition(infer_reg(mtcars, wt~drat))
})

test_that("regression fails with mispelling", {
  expect_error(infer_reg(mtcars, Drat~wt))
})


# test_chisq() ####

test_that("chisq works", {
  expect_no_condition(
    suppressWarnings(infer_chisq(mtcars, cyl~gear))
    )
})

test_that("chisq works with specifying test", {
  expect_no_condition(
    suppressWarnings(infer_chisq(mtcars, cyl~gear, type = "test"))
  )
})

test_that("chisq works with specifying expected", {
  expect_no_condition(
    suppressWarnings(infer_chisq(mtcars, cyl~gear, type = "expected"))
  )
})

test_that("chisq works with specifying observed", {
  skip("superseded?")
  expect_no_condition(
    suppressWarnings(infer_chisq(mtcars, cyl~gear, type = "observed"))
  )
})

test_that("chisq works with specifying observed and digits", {
  skip("superseded?")
  expect_no_condition(
    suppressWarnings(infer_chisq(mtcars, cyl~gear, type = "observed", digits = 4))
  )
})

test_that("chisq works with specifying observed and caption", {
  skip("superseded?")
  expect_no_condition(
    suppressWarnings(infer_chisq(mtcars, cyl~gear, type = "observed", caption = "hehe"))
  )
})

test_that("chisq works with digits", {
  expect_no_condition(
    suppressWarnings(infer_chisq(mtcars, cyl~gear, type = "test", digits = 4))
  )
})

test_that("chisq works with digits and expected", {
  expect_no_condition(
    suppressWarnings(infer_chisq(mtcars, cyl~gear, type = "expected", digits = 4))
  )
})

test_that("chisq works with caption", {
  expect_no_condition(
    suppressWarnings(infer_chisq(mtcars, cyl~gear, type = "test", caption = "yeah"))
  )
})

test_that("chisq works with caption and expected", {
  expect_no_condition(
    suppressWarnings(infer_chisq(mtcars, cyl~gear, type = "test", caption = "hehe"))
  )
})

test_that("chisq works with different order", {
  expect_no_condition(
    suppressWarnings(infer_chisq(mtcars, gear~cyl, type = "test", caption = "hehe"))
  )
})

test_that("chisq fails with mispelling", {
  expect_error(
    suppressWarnings(infer_chisq(mtcars, Cyl~gear, type = "test"))
  )
})

test_that("anova errors with same var", {
  expect_error(supressWarnings(infer_anova(mtcars, cyl~cyl)))
})

# infer_anova() ####

test_that("anova works", {
  skip("superceded?")
  expect_no_condition(infer_anova(mtcars, cyl~gear))
})

test_that("anova works with digits", {
  skip("superceded?")
  expect_no_condition(infer_anova(mtcars, cyl~gear, digits = 4))
})

test_that("anova works with caption", {
  skip("superceded?")
  expect_no_condition(infer_anova(mtcars, cyl~gear, caption = "hehe"))
})

test_that("anova works with reorder", {
  skip("superceded?")
  expect_no_condition(infer_anova(mtcars, gear~cyl))
})

test_that("anova warns with same var", {
  skip("superceded?")
  expect_error(suppressWarnings(infer_anova(mtcars, cyl~cyl)))
})

test_that("anova fails with mispelling", {
  skip("superceded?")
  expect_no_condition(infer_anova(mtcars, Cyl~gear))
})



