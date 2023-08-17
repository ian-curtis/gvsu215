# tbl_one() ####
test_that("one way generates", {
  skip("superseded")
  expect_no_condition(tbl_one(mtcars, ~gear))
})

test_that("one way generates with digits", {
  skip("superseded")
  expect_no_condition(tbl_one(mtcars, ~gear, digits = 2))
})

test_that("one way generates with caption", {
  skip("superseded")
  expect_no_condition(tbl_one(mtcars, ~gear, caption = "hehe"))
})

test_that("one way fails with mispelling", {
  expect_error(tbl_one(mtcars, ~Gear))
})


# tbl_two() ####
test_that("two way generates", {
  skip("superseded")
  expect_no_condition(tbl_two(mtcars, cyl~gear))
})

test_that("two way generates with caption", {
  skip("superseded")
  expect_no_condition(tbl_two(mtcars, cyl~gear, caption = "hehe"))
})

test_that("two way generates with digits", {
  skip("superseded")
  expect_no_condition(tbl_two(mtcars, cyl~gear, digits = 2))
})

test_that("two way generates with row percents", {
  skip("superseded")
  expect_no_condition(tbl_two(mtcars, cyl~gear, row_pct = TRUE))
})

test_that("two way fails with wrong row percent argument", {
  expect_error(tbl_two(mtcars, cyl~gear, row_pct = "yes"))
})

test_that("two way fails with mispelling", {
  expect_error(tbl_two(mtcars, Cyl~Gear))
})
