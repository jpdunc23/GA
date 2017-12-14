context("Tests for tournament and proportional")

X <- mtcars[-1]
y <- unlist(mtcars[1])
index <-initialize_parents(10,20)$index
dict.fitness <<- new.env()
models <- ranked_models(index, X, y)
models_err <- models
models_err$error <- 1:nrow(models_err)

test_that("check the validity of input",{
  # Test k
  expect_error(tournament(models, k=3.5))
  expect_error(tournament(models, k=nrow(models)+3))

  # Test models
  expect_error(tournament(models_err, k=2))
  expect_error(propotional(models_err))
  expect_error(propotional(c(1,3,5)))
})
