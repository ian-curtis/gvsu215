# one_way() ####
test_that("one way generates", {
  expect_no_condition(one_way(mtcars, ~gear))
})

test_that("one way generates with digits", {
  expect_no_condition(one_way(mtcars, ~gear, digits = 2))
})

test_that("one way generates with caption", {
  expect_no_condition(one_way(mtcars, ~gear, caption = "hehe"))
})

test_that("one way fails with mispelling", {
  expect_error(one_way(mtcars, ~Gear))
})


# two_way() ####
test_that("two way generates", {
  expect_no_condition(two_way(mtcars, cyl~gear))
})

test_that("two way generates with caption", {
  expect_no_condition(two_way(mtcars, cyl~gear, caption = "hehe"))
})

test_that("two way generates with digits", {
  expect_no_condition(two_way(mtcars, cyl~gear, digits = 2))
})

test_that("two way generates with row percents", {
  expect_no_condition(two_way(mtcars, cyl~gear, row_pct = TRUE))
})

test_that("two way fails with wrong row percent argument", {
  expect_error(two_way(mtcars, cyl~gear, row_pct = "yes"))
})

test_that("two way fails with mispelling", {
  expect_error(two_way(mtcars, Cyl~Gear))
})
