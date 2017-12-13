library(testthat)
require(library)

X <- mtcars[-1]
y <- unlist(mtcars[1])
y_err <- y[-2]
index <-initialize_parents(10,20)$index
dict.fitness <<- new.env()
cores <- parallel::detectCores()
cluster <- parallel::makeCluster(cores)

test_that("check the validity of input",{

  expect_error(ranked_models(index, 3, y))
  expect_error(ranked_models(index, X, y_err))
  expect_error(ranked_models(index, X, X))
  expect_error(ranked_models(index, X, y, "KIC"))
  
})
