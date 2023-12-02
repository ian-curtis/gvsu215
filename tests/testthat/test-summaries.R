# tbl_num_sum() ####
test_that("num sum works when not removing NAs and no NAs are present", {
  expect_no_condition(tbl_num_sum(mtcars, ~wt))
})

test_that("num sum works with NAs removed", {
  expect_no_condition(tbl_num_sum(mtcars, ~wt, na_rm = TRUE))
})

test_that("num sum works with digits", {
  expect_no_condition(tbl_num_sum(mtcars, ~wt, na_rm = TRUE, digits = 2))
})

test_that("num sum works with digits", {
  expect_no_condition(tbl_num_sum(mtcars, ~wt, na_rm = TRUE, digits = 2))
})

test_that("num sum works with caption", {
  expect_no_condition(tbl_num_sum(mtcars, ~wt, na_rm = TRUE, caption = "hehe"))
})

test_that("num sum works with groups", {
  expect_no_condition(tbl_num_sum(mtcars, wt~cyl, na_rm = TRUE))
})

test_that("num sum fails with mispelling", {
  expect_error(tbl_num_sum(mtcars, ~WT, na_rm = TRUE, digits = 2))
})

test_that("num sum fails with groups and mispellling", {
  expect_error(tbl_num_sum(mtcars, Wt~Cyl, na_rm = TRUE, digits = 2))
})

test_that("num sum warns with NAs and na_rm = FALSE", {
  expect_condition(tbl_num_sum(airquality, ~Ozone, na_rm = FALSE))
})

test_that("num sum warns with NAs and na_rm = FALSE with groups", {
  expect_condition(tbl_num_sum(airquality, Ozone~Month, na_rm = FALSE))
})


# tbl_pctile() ####

# test_that("tbl_pctile displays message when NAs are not removed", {
#   expect_warning(tbl_pctile(mtcars, ~wt))
# })

test_that("tbl_pctile displays works when NAs are removed", {
  expect_no_error(tbl_pctile(mtcars, ~wt))
})

test_that("tbl_pctile works with new probs", {
  expect_no_error(tbl_pctile(mtcars, ~wt, probs = c(.17, .3, .5, .7, .9, 1)))
})

test_that("tbl_pctile works with caption", {
  expect_no_condition(tbl_pctile(mtcars, ~wt, caption = "hehe"))
})

test_that("tbl_pctile works with groups", {
  expect_no_condition(tbl_pctile(mtcars, wt~cyl))
})

test_that("tbl_pctile works with groups and caption", {
  expect_no_condition(tbl_pctile(mtcars, wt~cyl, caption = "hehe"))
})

test_that("tbl_pctile works with groups and probs", {
  expect_no_condition(tbl_pctile(mtcars, wt~cyl, probs = c(.17, .3, .5, .7, .9, 1)))
})

test_that("tbl_pctile fails with mispelling", {
  expect_error(tbl_pctile(mtcars, Wt~Cyl))
})

test_that("tbl_pctile fails with invalid probs", {
  expect_error(tbl_pctile(mtcars, ~cyl, probs = c(-1, 5)))
})


# tbl_corr() ####

test_that("corr informs with NAs not removed", {
  expect_condition(tbl_corr(mtcars, wt~qsec))
})

test_that("corr works with NAs removed", {
  expect_no_condition(tbl_corr(mtcars, wt~qsec, na_rm = TRUE))
})

test_that("corr fails with mispelling", {
  expect_error(tbl_corr(mtcars, Wt~qsec, na_rm = TRUE))
})




