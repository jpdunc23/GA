select <- function(X, y, randomness = TRUE, generation_count=2 * ncol(X), error_func=AIC, k){
  #' Ranked each model by its AIC,
  #' Choose parents from generations propotional to their fitness
  #' Do crossover and mutation
  #' Replace k worst old individuals by best k new individuals
  #' @param old_gen: dataframe with index and AIC
  #' @param X: dataframe containing vairables in the model
  #' @param y: vector targeted variable
  #' @param random: if TURE, one parent will be selected randomly
  #' @return The selected parents(list of n lists, each list contains two parents)
  #' @examples
  #' x <- mtcars[-1]
  #' y <- unlist(mtcars[1])
  #' next_gen <- select(X, y, randomness=TRUE, k=5, generation_count=100, error_func=BIC)

  parents <- initialize_parents(ncol(X), generation_count=generation_count)
  old_gen <- ranked_models(parents, X, y)

  while (nrow(next_gen) > 1) {
    ##### select parents #####

    parents <- propotional(old_gen, random = randomness)

    ##### crossover and mutation #####

    children <- lapply(parents, breed, p=length(x))

    ##### ranked new generation and calculate AIC #####

    ranked_new <- ranked_models(children, X, y)

    ##### replace k worst old individuals with k new individuals #####

    next_gen <- generation_gap(ranked_new, old_gen, k)
  }
  return(next_gen)
}
