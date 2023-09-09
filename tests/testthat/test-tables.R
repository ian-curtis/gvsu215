# tbl_1var() ####
test_that("one way generates", {
  # skip("superseded")
  expect_no_warning(tbl_1var(mtcars, ~gear))
})

test_that("one way generates with digits", {
  # skip("superseded")
  expect_no_warning(tbl_1var(mtcars, ~gear, digits = 2))
})

test_that("one way generates with caption", {
  # skip("superseded")
  expect_no_warning(tbl_1var(mtcars, ~gear, caption = "hehe"))
})

test_that("one way fails with mispelling", {
  expect_error(tbl_1var(mtcars, ~Gear))
})


# tbl_2var() ####
test_that("two way generates", {
  # skip("superseded")
  expect_no_warning(tbl_2var(mtcars, cyl~gear))
})

test_that("two way generates with caption", {
  # skip("superseded")
  expect_no_warning(tbl_2var(mtcars, cyl~gear, caption = "hehe"))
})

test_that("two way generates with digits", {
  # skip("superseded")
  expect_no_warning(tbl_2var(mtcars, cyl~gear, digits = 2))
})

test_that("two way generates with row percents", {
  # skip("superseded")
  expect_no_warning(tbl_2var(mtcars, cyl~gear, row_pct = TRUE))
})

test_that("two way fails with wrong row percent argument", {
  expect_error(tbl_2var(mtcars, cyl~gear, row_pct = "yes"))
})

test_that("two way fails with mispelling", {
  expect_error(tbl_2var(mtcars, Cyl~Gear))
})
