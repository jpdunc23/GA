context("select tests")

X <- mtcars[-1]
y <- mtcars[, 1]

test_that("Test for correct output of select with various inputs", {
  expect_true(is.list(select(X, y, max_iter = 30)))
  expect_true(is.list(select(X, y, family = poisson, max_iter = 30)))
  expect_true(is.list(select(X, y, K = 5, max_iter = 30)))  
  expect_true(is.list(select(X, y, selection = "proportional", max_iter = 30)))
  expect_true(is.list(select(X, y, selection = "proportional", randomness = FALSE,
                             max_iter = 30)))
  expect_true(is.list(select(X, y, fit_func = BIC, max_iter = 30)))
  expect_true(is.list(select(X, y, parallel = FALSE, max_iter = 30)))
})
