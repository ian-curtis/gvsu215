# infer_reg() ####

test_that("regression works", {
  expect_no_condition(infer_reg(mtcars, drat~wt))
})

test_that("regression works with not reduced", {
  expect_no_condition(infer_reg(mtcars, drat~wt, reduced = "no"))
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
  # skip("superseded?")
  expect_no_warning(
    suppressWarnings(infer_chisq(mtcars, cyl~gear, type = "observed"))
  )
})

test_that("chisq works with specifying observed and digits", {
  # skip("superseded?")
  expect_no_warning(
    suppressWarnings(infer_chisq(mtcars, cyl~gear, type = "observed", digits = 4))
  )
})

test_that("chisq works with specifying observed and caption", {
  # skip("superseded?")
  expect_no_warning(
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

# infer_anova() ####

test_that("anova works", {
  # skip("superceded?")
  expect_no_warning(infer_anova(mtcars, cyl~gear))
})

test_that("anova works with digits", {
  # skip("superceded?")
  expect_no_warning(infer_anova(mtcars, cyl~gear, digits = 4))
})

test_that("anova works with caption", {
  # skip("superceded?")
  expect_no_warning(infer_anova(mtcars, cyl~gear, caption = "hehe"))
})

test_that("anova works with reorder", {
  # skip("superceded?")
  expect_no_warning(infer_anova(mtcars, gear~cyl))
})

test_that("anova warns with same var", {
  # skip("superceded?")
  expect_error(suppressWarnings(infer_anova(mtcars, cyl~cyl)))
})

test_that("anova fails with mispelling", {
  expect_error(infer_anova(mtcars, Cyl~gear))
})




