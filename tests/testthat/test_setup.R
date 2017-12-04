test_that("Input is sanitized", {
  expect_error(initialize_parents("a"))
  expect_that(initialize_parents(10.0), is_a("list"))
  expect_that(initialize_parents(10.0, generation_count = 10.0), is_a("list"))
})
