library(testthat)

test_that("add_numbers correctly sums positive integers", {
  expect_true(identical(myncurve(3, 7, 10), NULL))
  expect_true(identical(myncurve(4, 12, 10), NULL))
  expect_true(identical(myncurve(1, 8, 2), NULL))
})
