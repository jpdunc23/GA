context("select tests")

test_that("Basic test cases", {
  expect_that(next_gen <- select(mtcars[-1], unlist(mtcars[1]), randomness=TRUE, k=5, generation_count=100, error_func=BIC), is_a("list"))
})
