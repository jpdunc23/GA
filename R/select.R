select <- function(X, y, C = ncol(X), randomness = TRUE, generation_count=2 * ncol(X), G = 1){
  #' Ranked each model by its AIC,
  #' Choose parents from generations propotional to their fitness
  #' Do crossover and mutation
  #' Replace k worst old individuals by best k new individuals
  #' @param X: dataframe containing vairables in the model
  #' @param y: vector targeted variable
  #' @param C: number of random point when crossover
  #' @param randomness: if TURE, one parent will be selected randomly
  #' @param generation_count: number of generations to initialize
  #' @param G: number of worst-performing paretns the user wishes to replace by best parents 
  #' @return The converged generation.
  #' @examples
  #' x <- mtcars[-1]
  #' y <- unlist(mtcars[1])
  #' finial_gen <- select(x, y, randomness=TRUE, G=2)
  
  feature_count <- ncol(X)
  initial <- initialize_parents(ncol(X), generation_count=generation_count)
  old_gen <- ranked_models(initial$index, X, y)
  AIC <- old_gen$AIC
  i <- 0   # number of iterations
  while(identical(AIC,rep(AIC[1],length(AIC)))==FALSE){
    #print(old_gen)
    ##### select parents #####

    parents <- propotional(old_gen, random = randomness)

    ##### crossover and mutation #####

    children <- unlist(lapply(parents, breed, C),FALSE, FALSE)

    ##### ranked new generation and calculate AIC #####

    ranked_new <- ranked_models(children, X, y)

    ##### replace k worst old individuals with k new individuals #####

    next_gen <- generation_gap(ranked_new, old_gen,G)
    
    ##### let new genration reproudce next offspring ######
    old_gen <- next_gen
    AIC <- old_gen$AIC
    i <- i + 1
  }
  summary <- list(final_gen = old_gen$Index[[1]],AIC = AIC[[1]],num_iteration = i)
  return(summary)
}


