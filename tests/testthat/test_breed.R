context("breed and crossover tests")

## randomized test input
C <- sample(1:20, 1)
n1 <- sample(1:C, 1)
n2 <- sample(1:C, 1)
parent1 <- sort(sample(1:C, n1))
parent2 <- sort(sample(1:C, n2))
parents <- list(parent1, parent2)

test_that("n >= C results in warning", {
    expect_warning(breed(parents, C, C))
})

test_that("breed returns correct output", {
    children <- breed(parents, C)
    expect_output(str(children), "List of 2")
    child1 <- children[[1]]
    child2 <- children[[2]]
    expect_true(is.numeric(child1) && is.numeric(child2))
    expect_true(length(child1) <= C)
    expect_true(length(child2) <= C)
    expect_true(all(child1 <= C) && all(child2 <= C))
})

test_that("crossover returns correct output", {
    splits <- 2
    parent1 <- c(1, 4, 7)
    parent2 <- c(3, 8, 11)
    child1 <- crossover(splits, parent1, parent2)
    child2 <- crossover(splits, parent2, parent1)
    expect_equal(child1, c(1, 3, 8, 11))
    expect_equal(child2, c(4, 7))

    splits <- c(2, 4, 8)
    parent1 <- c(1, 4, 7, 13)
    parent2 <- c(3, 8, 11, 17)
    child1 <- crossover(splits, parent1, parent2)
    child2 <- crossover(splits, parent2, parent1)
    expect_equal(child1, c(1, 3, 7, 11, 17))
    expect_equal(child2, c(4, 8, 13))

    splits <- c(1, 2, 3, 4, 5, 6)
    parent1 <- c(1, 2, 5)
    parent2 <- c(2, 4, 7)
    child1 <- crossover(splits, parent1, parent2)
    child2 <- crossover(splits, parent2, parent1)
    expect_equal(child1, c(1, 2, 4, 5))
    expect_equal(child2, c(2, 7))

    splits <- c(1, 3, 4, 7, 10, 21)
    parent1 <- c(1, 2, 3, 4, 5)
    parent2 <- c(1, 2, 3, 4, 5)
    child1 <- crossover(splits, parent1, parent2)
    child2 <- crossover(splits, parent2, parent1)
    expect_equal(child1, c(1, 2, 3, 4, 5))
    expect_equal(child2, c(1, 2, 3, 4, 5))
    
})
