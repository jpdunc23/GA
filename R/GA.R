GA <- function(X, y, num_iter = 100) {
  
  # Kunal
  # Create starting set of parents
  # Input: # of columns in X = p
  # Output: Multiple random string of feature indexes ex. [0, 3, 4, 5, 10], multiple binary string with 0s where indexes aren't included and 1 where indexes are included of size 2 * P

  # for num_iter:
    
    # Kunal
    # For parent in generation:
      # Calculate AIC of user-defined fit criteria
      # Input: X (with selected features), y
      # Output: AIC
  
    # Fan
    # Sort parents in the generation
    # Input: List of parents and fit
    # Output: Sorted list of parents
  
    # Xin
    # Get new parents
    # Input: Generation
    # Output: Tuple of parents
  
    # James
    # Crossover and Mutation
    # Input: Takes in tuple of parents
    # Output: Returns new generation
  
    # parent = new_generation
  # return variable selection
}

#' Breeding to create a new combination of predictors to use in
#'   the regression, via genetic crossover and mutation by default.
#'
#' @param parents A tuple (2-list) of ordered numeric vectors,
#'   representing the parents which will breed. Each parent is
#'   represented by the indices of the genes which are active
#'   in its chromosome, e.g. c(1, 4, 7) corresponds to using the
#'   first, fourth, and seventh predictors in the regression.
#' @param C The length of chromosomes, i.e. the maximum number of
#'   possible predictors.
#' @param n Number of crossover points, up to a maximum of C - 1.
#' @param op An optional, user-specified genetic operator function
#'   to carry out the breeding.
#' @param ... Additional parameters to pass to the user-specific op
#'   function.
#' @return A tuple (2-list) of ordered numeric vector representing
#'   the two offspring produced by the breeding as indices of the
#'   genes (predictors) which are active in the chromosome (regression).
#' @examples
#' C <- 5 ## 5 genes / max number of predictors
#' ## parent generation of size 3
#' parent_gen <- list(list(c(1, 3), c(4)),
#'                    list(c(2, 3), c(1,4)),
#'                    list(c(3), c(1, 3, 4)))
#' ## list of numeric vectors representing the next generation
#' next_gen <- unlist(lapply(parent_gen, breed, C), FALSE, FALSE)
breed <- function(parents, C, n = 1, op = NULL, ...) {
    if (n >= C - 1) {
        msg <- paste0("Number of crossover points is greater than ",
                      "chromosome length. Using default number of ",
                      "crossover points (1) instead.")
        warning(msg)
        n = 1
    }
    
    if (!is.null(op)) {
        ## run user-provided genetic operation
        return(op(parents, C, ...))
    } else {
        ## default breeding with random crossover point
        ## and mutation chance of 1% at each gene

        parent1 <- parents[[1]]
        parent2 <- parents[[2]]

        ## random crossover points
        splits <- sort(sample(1:(C - 1), n))
        embryo1 <- crossover(splits, parent1, parent2)
        embryo2 <- crossover(splits, parent2, parent1)

        ## each gene/parameter has a 1% chance to be flipped on/off
        mutate1 <- which(runif(C) <= 0.01)
        mutate2 <- which(runif(C) <= 0.01)

        ## combine genes of embryo that are not mutated
        ## with those that are activated by the mutation
        child1 <- sort(c(setdiff(embryo1, mutate1),
                         setdiff(mutate1, embryo1)))
        child2 <- sort(c(setdiff(embryo2, mutate2),
                         setdiff(mutate2, embryo2)))        

        if (length(child1) == length(child2) &&
            all(child1 == child2)) return(list(child1))
        else return(list(child1, child2))
    }

}

crossover <- function(splits, parent1, parent2) {
    n <- length(splits)
    unlist(sapply(1:(n + 1), function(i) {
        ##browser()
        if (i %% 2 == 1) {
            ## take genetic material from first parent
            if (i == 1) {
                parent1[parent1 <= splits[i]]
            } else if (i == n + 1) {
                parent1[parent1 > splits[n]]
            } else {
                parent1[parent1 > splits[i - 1] &
                        parent1 <= splits[i]]
            }
        } else {
            ## take genetic material from second parent
            if (i == n + 1) {
                parent2[parent2 > splits[n]]
            } else {
                parent2[parent2 > splits[i - 1] &
                        parent2 <= splits[i]]
            }
        }
    }), FALSE, FALSE)
}
