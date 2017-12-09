library(dplyr)
library(assertive)
library(parallel)
###########################################################################################
# Function: select
#
#' Ranked each model by its fitness,
#' Choose parents from generations propotional to their fitness
#' Do crossover and mutation
#' Replace a proportion G of the worst old individuals by best
#'   new individuals
#' @param X: dataframe containing vairables in the model
#' @param y: vector targeted variable
#' @param C The length of chromosomes, i.e. the maximum number of
#'   possible predictors.
#' @param family: a description of the error distribution and link function to be used in gm.
#' @param selection: selection mechanism. Can be either "proportional" or "tournament".
#' @param K: size of each round of selection when using tournament selection.
#'   Must be an integer smaller than generation size.
#' @param randomness: if TURE, one parent will be selected randomly
#' @param P: population size
#' @param G: proportion of worst-performing parents the user wishes to replace by best offspring
#' @param n_splits: number of crossover points to use in breeding
#' @param op: An optional, user-specified genetic operator function
#'   to carry out the breeding.
#' @param fit_func: Function for fitness measurement. Default is AIC.
#' @param max_iter: how many iterations to run before stopping
#' @return The best individual seen over all iterations.
#' @examples
#' x <- mtcars[-1]
#' y <- unlist(mtcars[1])
#' select(x, y, selection = "tournament", K = 5, randomness=TRUE, G=0.8)
#' @export

select <- function(X, y, C = ncol(X), family = gaussian,
                   selection = "tournament", K = 2,
                   randomness = TRUE, P = 2 * ncol(X),
                   G = 1/P, n_splits = 2, op = NULL,
                   fit_func = AIC, max_iter = 100, parallel=TRUE, ...) {

  feature_count <- ncol(X)
  dict.fitness <<- new.env()
  initial <- initialize_parents(ncol(X), P)

  if (parallel) {
    cores <- detectCores()
    cluster <- makeCluster(cores)
    clusterExport(cluster, "crossover")
    clusterExport(cl = cluster, c("dict.fitness"), envir = dict.fitness)
  } else {
    cluster = NA
  }


  old_gen <- ranked_models(initial$index, X, y, fit_func, cluster=cluster)
  fitness <- old_gen$fitness

  best <- c() ## best seen so far
  best_i <- 0 ## iteration when it was seen
  best_fit <- Inf ## fitness of best so far

  i <- 0   # number of iterations
  while(i < max_iter) {

    ##### select parents #####

    if (selection == "proportional"){
      parents <- propotional(old_gen, random = randomness)
    } else {
      parents <- tournament(old_gen, k=K)
    }

    ##### crossover and mutation #####
    if (parallel && !is.na(cluster)) {
      children <- unique(unlist(parLapply(cluster, parents, breed, C, n_splits, op),
                                FALSE, FALSE))
    } else {
      children <- unique(unlist(lapply(parents, breed, C, n_splits, op),
                                FALSE, FALSE))
    }


    ##### ranked new generation and calculate fitness #####

    ranked_new <- ranked_models(children, X, y, fit_func, cluster=cluster)

    ##### replace k worst old individuals with k new individuals #####

    next_gen <- generation_gap(old_gen, ranked_new, G)

    ## update our best so far if necessary
    if (next_gen$fitness[1] < best_fit) {
      best_fit <- next_gen$fitness[1]
      best <- next_gen$Index[[1]]
      best_i <- i + 1
    }

    ##### let new genration reproudce next offspring ######
    old_gen <- next_gen
    fitness <- old_gen$fitness
    i <- i + 1
  }
  if (!is.na(cluster)) {
    stopCluster(cluster)
  }

  summary <- list(survivor = best, fitness = best_fit,
                  num_iteration = i, first_seen = best_i)
  detach("package:dplyr", unload=TRUE)
  return(summary)
}
