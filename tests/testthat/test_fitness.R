context("calculation_fitness tests")

dict.fitness <<- new.env()
test_that("Input is sanitized", {
  expect_error(calculate_fitness("a", "y"))
})
dict.fitness <<- new.env()
test_that("Normal input", {
  expect_that(calculate_fitness(1:3, data.frame(replicate(10,sample(0:1,1000,rep=TRUE))), 1:1000, fit_func=AIC), is_a("numeric"))
})
dict.fitness <<- new.env()
bad <- function(a, b, c) {
  return("a")
}
test_that("User input error function doesn't return a numeric", {
  expect_error(calculate_fitness(1:3, data.frame(replicate(10,sample(0:1,1000,rep=TRUE))), 1:1000, fit_func=bad))
})
dict.fitness <<- new.env()
test_that("X and y have different numbers of rows and columns", {
  expect_error(calculate_fitness(1:3, data.frame(replicate(10,sample(0:1,1100,rep=TRUE))), 1:1000, AIC))
})
