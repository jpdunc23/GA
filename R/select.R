<<<<<<< HEAD
select <- function(X, y, randomness = TRUE, generation_count=2 * ncol(X), k = generation_count){
=======
select <- function(X, y, C = ncol(X), randomness = TRUE, generation_count=2 * ncol(X), G = 1){
>>>>>>> c54231a17bbecbeb94098f61f6f2428b65d056d7
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
<<<<<<< HEAD
  #' next_gen <- select(X, y, randomness=TRUE, k=5, generation_count=100, error_func=BIC)

  parents <- initialize_parents(ncol(X), generation_count=generation_count)
  old_gen <- ranked_models(parents, X, y)

  while (nrow(old_gen) > 1) {
=======
  #' finial_gen <- select(x, y, randomness=TRUE, G=2)
  
  feature_count <- ncol(X)
  initial <- initialize_parents(ncol(X), generation_count=generation_count)
  old_gen <- ranked_models(initial$index, X, y)
  AIC <- old_gen$AIC
  i <- 0   # number of iterations
  while(identical(AIC,rep(AIC[1],length(AIC)))==FALSE){
    #print(old_gen)
>>>>>>> c54231a17bbecbeb94098f61f6f2428b65d056d7
    ##### select parents #####

    parents <- propotional(old_gen, random = randomness)

    ##### crossover and mutation #####

<<<<<<< HEAD
    children <- unlist(lapply(parents, breed, ncol(X)),
                       FALSE, FALSE)
=======
    children <- unlist(lapply(parents, breed, C),FALSE, FALSE)
>>>>>>> c54231a17bbecbeb94098f61f6f2428b65d056d7

    ##### ranked new generation and calculate AIC #####

    ranked_new <- ranked_models(children, X, y)

    ##### replace k worst old individuals with k new individuals #####

<<<<<<< HEAD
    next_gen <- generation_gap(ranked_new, old_gen, k)
    old_gen <- next_gen
=======
    next_gen <- generation_gap(ranked_new, old_gen,G)
    
    ##### let new genration reproudce next offspring ######
    old_gen <- next_gen
    AIC <- old_gen$AIC
    i <- i + 1
>>>>>>> c54231a17bbecbeb94098f61f6f2428b65d056d7
  }
  summary <- list(final_gen = old_gen$Index[[1]],AIC = AIC[[1]],num_iteration = i)
  return(summary)
}


