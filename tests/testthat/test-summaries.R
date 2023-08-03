# num_sum() ####
test_that("num sum warns when not removing NAs", {
  expect_warning(num_sum(mtcars, ~wt))
})

test_that("num sum works with NAs removed", {
  expect_no_condition(num_sum(mtcars, ~wt, na_rm = TRUE))
})

test_that("num sum works with digits", {
  expect_no_condition(num_sum(mtcars, ~wt, na_rm = TRUE, digits = 2))
})

test_that("num sum works with digits", {
  expect_no_condition(num_sum(mtcars, ~wt, na_rm = TRUE, digits = 2))
})

test_that("num sum works with caption", {
  expect_no_condition(num_sum(mtcars, ~wt, na_rm = TRUE, caption = "hehe"))
})

test_that("num sum works with groups", {
  expect_no_condition(num_sum(mtcars, wt~cyl, na_rm = TRUE))
})

test_that("num sum fails with mispelling", {
  expect_error(num_sum(mtcars, ~WT, na_rm = TRUE, digits = 2))
})

test_that("num sum fails with groups and mispellling", {
  expect_error(num_sum(mtcars, Wt~Cyl, na_rm = TRUE, digits = 2))
})


# pctile() ####

# test_that("pctile displays message when NAs are not removed", {
#   expect_warning(pctile(mtcars, ~wt))
# })

test_that("pctile displays works when NAs are removed", {
  expect_no_condition(pctile(mtcars, ~wt, na_rm = TRUE))
})

test_that("pctile works with new probs", {
  expect_no_condition(pctile(mtcars, ~wt, probs = c(.17, .3, .5, .7, .9, 1), na_rm = TRUE))
})

test_that("pctile works with caption", {
  expect_no_condition(pctile(mtcars, ~wt, caption = "hehe", na_rm = TRUE))
})

test_that("pctile works with groups", {
  expect_no_condition(pctile(mtcars, wt~cyl, na_rm = TRUE))
})

test_that("pctile works with groups and caption", {
  expect_no_condition(pctile(mtcars, wt~cyl, na_rm = TRUE, caption = "hehe"))
})

test_that("pctile works with groups and probs", {
  expect_no_condition(pctile(mtcars, wt~cyl, na_rm = TRUE, probs = c(.17, .3, .5, .7, .9, 1)))
})

test_that("pctile fails with mispelling", {
  expect_error(pctile(mtcars, Wt~Cyl, na_rm = TRUE))
})

test_that("pctile fails with invalid probs", {
  expect_error(pctile(mtcars, ~cyl, probs = c(-1, 5)))
})


# corr() ####

test_that("corr warns with NAs not removed", {
  expect_warning(corr(mtcars, wt~qsec))
})

test_that("corr works with NAs removed", {
  expect_no_condition(corr(mtcars, wt~qsec, na_rm = TRUE))
})

test_that("corr fails with mispelling", {
  expect_error(corr(mtcars, Wt~qsec, na_rm = TRUE))
})



