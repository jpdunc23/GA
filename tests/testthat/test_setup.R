library(testthat)
test_that("Input is sanitized", {
  expect_error(initialize_parents("a"))
  expect_error(initialize_parents(NA))
  expect_that(initialize_parents(10.0), is_a("list"))
})

test_that("Sanity check", {
  expect_that(initialize_parents(10.0, P=100), is_a("list"))
})

test_that("0 features sampled", {
  expect_that(initialize_parents(10.0, P=0), is_a("list"))
})

test_that("0 features chosen", {
  expect_that(initialize_parents(0, P=0), is_a("list"))
})
