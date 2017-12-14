context("generation_gap tests")

X <- mtcars[-1]
y <- mtcars[, 1]
C <- ncol(X)

dict.fitness <- new.env()
parents_init <- initialize_parents(C)
ranked_parents <- ranked_models(parents_init$index, X, y)
pairs1 <- propotional(ranked_parents, TRUE)
pairs2 <- propotional(ranked_parents, FALSE)
pairs3 <- tournament(ranked_parents, 2)

children1 <- ranked_models(unique(unlist(lapply(pairs1, breed, C),
                           FALSE, FALSE)), X, y)
children2 <- ranked_models(unique(unlist(lapply(pairs2, breed, C),
                           FALSE, FALSE)), X, y)
children3 <- ranked_models(unique(unlist(lapply(pairs3, breed, C),
                           FALSE, FALSE)), X, y)

test_that("G <= 0 and G > 1 result in warnings", {
    expect_warning(generation_gap(ranked_parents, children1, 0))
    expect_warning(generation_gap(ranked_parents, children1, 2))    
})

test_that("generation_gap returns correct output", {
    o1 <- generation_gap(ranked_parents, children1)
    o2 <- generation_gap(ranked_parents, children2)
    o3 <- generation_gap(ranked_parents, children3)
    expect_true(is.data.frame(o1))
    expect_true(is.data.frame(o2))
    expect_true(is.data.frame(o3))
    expect_equal(nrow(o1), nrow(ranked_parents))
    expect_equal(nrow(o2), nrow(ranked_parents))
    expect_equal(nrow(o3), nrow(ranked_parents))
})

test_that("generation_gap handles differing values of G correctly", {
    o1 <- generation_gap(ranked_parents, children3, 0.25)
    o2 <- generation_gap(ranked_parents, children3, 0.5)
    o3 <- generation_gap(ranked_parents, children3, 0.75)
    expect_true(is.data.frame(o1))
    expect_true(is.data.frame(o2))
    expect_true(is.data.frame(o3))
    expect_equal(nrow(o1), nrow(ranked_parents))
    expect_equal(nrow(o2), nrow(ranked_parents))
    expect_equal(nrow(o3), nrow(ranked_parents))
})
