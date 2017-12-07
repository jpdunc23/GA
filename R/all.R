initialize_parents <- function(feature_count, generation_count=2*feature_count) {
  ## inputs:
  ##   feature_count        Number of features given
  ##   generation_count     Number of parents in the generation
  ##
  ## output: Object with indexes of what features to include
  ##  $binary    list of list of [0, 1] where 0 means don't include features and 1 means include feature
  ##  $index     list of lists of column indexes to include
  ##
  ## Examples:
  ##  parents <- initialize_parents(10, generation_count = 10, number_features=4)
  
  # Sanitize input, ensure everything is an integer
  feature_count <- as.integer(feature_count)
  generation_count <- as.integer(generation_count)
  
  # Ensure input is sanitized correctly
  if (!is.integer(feature_count)) {
    stop("Feature count is not an integer")
  }
  if (!is.integer(generation_count)) {
    stop("Generation count is not an integer")
  }
  
  # index_list (returns list of features that are included)
  # binary_list (returns 1 if feature is included, otherwise 0)
  index_list <- list()
  binary_list <- list()
  
  # creates all the parents in the generation
  while (length(index_list) < generation_count) {
    
    # samples a fixed number of features from the total features
    indexes <- sort(sample(c(1:feature_count), size = sample(0:feature_count, 1)))
    index_length <- length(index_list)
    index_list[[index_length + 1]] <- indexes
    
    binary_string <- rep(0, feature_count)
    binary_string[indexes] <- 1
    binary_length <- length(binary_list)
    binary_list[[binary_length + 1]] <- binary_string
    
    # Check for duplicates!
    index_list <- unique(index_list)
    binary_list <- unique(binary_list)
  }
  
  return (list("binary" = binary_list, "index" = index_list))
}

library(assertive)
library(plyr)

initialize_parents <- function(feature_count, generation_count=100) {
  ## inputs:
  ##   feature_count        Number of features given
  ##   generation_count     Number of parents in the generation
  ##
  ## output: Object with indexes of what features to include
  ##  $binary    list of list of [0, 1] where 0 means don't include features and 1 means include feature
  ##  $index     list of lists of column indexes to include
  ##
  ## Examples:
  ##  parents <- initialize_parents(10, generation_count = 10)
  
  # Sanitize input, ensure everything is an integer
  feature_count <- as.integer(feature_count)
  generation_count <- as.integer(generation_count)
  
  # Ensure input is sanitized correctly
  is_integer(feature_count)
  is_integer(generation_count)
  
  # index_list (returns list of features that are included)
  # binary_list (returns 1 if feature is included, otherwise 0)
  index_list <- list()
  binary_list <- list()
  
  # creates all the parents in the generation
  for (generation in 1:generation_count) {
    
    # samples a fixed number of features from the total features
    indexes <- sort(sample(c(1:feature_count), size = sample(0:feature_count, 1)))
    index_list[[generation]] <- indexes
    
    binary_string <- rep(0, feature_count)
    binary_string[indexes] <- 1
    binary_list[[generation]] <- binary_string
  }
  
  return (list("binary" = binary_list, "index" = index_list))
}


calculate_fitness <- function(index, X, y, error_func) {
  ## inputs:
  ##   X               Data frame of selected features
  ##   y               Output variable
  ##   error_func      Error function
  ##
  ## output: Value of error function
  ##  A numeric which determines the fit of the model
  ##
  ## Examples:
  ##  fitness <- calculate_fitness(data.frame(replicate(10,sample(0:1,1000,rep=TRUE))), 1:1000)
  index.str <- deparse(index)
  if (index.str %in% names(dict.fitness)) {
    return(get(index.str, envir = dict.fitness))
  } else{
    X <- X[,index]
    X <- as.data.frame(X)
    y <- as.vector(y)
    
    is_data.frame(X)
    is_vector(y)
    
    stopifnot(nrow(X) == length(y))
    
    X$y <- y
    
    model <- lm(y ~ ., data = X)
    fitness <- error_func(model)
    assign(index.str, fitness, envir = dict.fitness)
    return (fitness) 
  }
  
}

ranked_models <- function(index, X, y, error_func=AIC) {
  #' Fit the models and rank them by their fitness function.
  #'
  #' @param index A list of indices of selected variables. 
  #' @param X Data frame of all features
  #' @param y Dependent variable
  #' @param error_func Error function for fitness measurement. Default is AIC.
  #' @return a data frame containing index list and their respective AIC, sorted by AIC in ascending order
  #' @examples
  #' X <- mtcars[-1] 
  #' y <- unlist(mtcars[1])
  #' index <-initialize_parents(10,20)$index
  #' ranked_models(index, X, y)
  #' 
  
  fitness <- lapply(index, calculate_fitness, X, y, error_func)
  model_fitness <- data.frame(sapply(list(index), `[`))
  colnames(model_fitness) <- c('Index')
  model_fitness$fitness <- unlist(fitness)
  model_fitness <- arrange(model_fitness,fitness)
  return(model_fitness)
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

propotional <- function(models,random = TRUE){
  #' Choose parents from generations propotional to their fitness
  #' @param models: dataframe with indexes and fitness
  #' @param random: if TURE, one parent will be selected randomly
  #' @return The selected parents(list of n lists, each list contains two parents)
  #' @examples
  #' x <- mtcars[-1]
  #' y <- unlist(mtcars[1])
  #' initial_index <- initialize_parents(10,20)$index
  #' rankedmodels <- ranked_models(initial_index,x,y)
  #' propotional(rankedmodels,random = T)
  index <- models$index
  fitness <- unlist(models$fitness)
  n <- nrow(models)
  num_offspring <- ceiling(n/2)
  if(max(fitness)>0&&identical(fitness,rep(fitness[1],length(fitness)))==FALSE){
    fitness <- fitness - max(fitness)
  }
  weight <- fitness / sum(fitness)
  sample_weight <- round(weight * n * 100)
  sample <- c()
  sample<-rep(1:length(sample_weight),sample_weight)
  father_index <- sample(sample,num_offspring,replace = TRUE)
  if(random == TRUE){
    mother_index <- sample(sample,num_offspring,replace=TRUE)
  }
  else{
    mother_index <- sample(1:n,num_offspring,replace=TRUE)
  }
  father <- models[father_index,]
  mother <- models[mother_index,]
  parents <- list()
  for(i in 1:num_offspring){
    parents[[i]] <- list(father[[1]][[i]],mother[[1]][[i]])
  }
  return(parents)
}

library(dplyr)
#' Replace the worst-performing offsprings with their best-performing parents.
#' The function will automatically compare the G worst-performing offsprings with the G best-performing parents. Suppose among them, k parents outperform k offsprings; the parents will then replace the k offsprings.
#' 
#' @param old_gen Output of ranked_models() of the previous generation. A data frame.
#' @param new_gen Output of ranked_models() of the current generation. A data frame.
#' @param G Number of worst-performing offsprings the user wishes to replace. The default is the obeservation number of new_gen.
#' @return The updated new_gen.
#' @examples
#' generation_gap(old_df, new_df, 10)

generation_gap <- function(old_gen, new_gen, G=nrow(new_gen)) {
  new.fitness <- na.omit(new_gen$fitness[nrow(new_gen)-G+1:nrow(new_gen)])
  old_gen <- arrange(old_gen, desc(fitness))
  old.fitness <- na.omit(old_gen$fitness[nrow(old_gen)-G+1:nrow(old_gen)])
  G.adjusted <- G - which(new.fitness >= old.fitness)[1] + 1
  new_gen[(nrow(new_gen)-G.adjusted+1):nrow(new_gen),] <- old_gen[(nrow(old_gen)-G.adjusted+1):nrow(old_gen),]
  new_gen <- arrange(new_gen,fitness)
  return(new_gen)
}

select <- function(X, y, C = ncol(X), randomness = TRUE, generation_count=2 * ncol(X), G = 1){
  #' Ranked each model by its fitness,
  #' Choose parents from generations propotional to their fitness
  #' Do crossover and mutation
  #' Replace k worst old individuals by best k new individuals
  #' @param X: dataframe containing vairables in the model
  #' @param y: vector targeted variable
  #' @param C: number of random point when crossover
  #' @param randomness: if TURE, one parent will be selected randomly
  #' @param generation_count: number of generations to initialize
  #' @param G: number of worst-performing paretns the user wishes to replace by best offspring 
  #' @return The converged generation.
  #' @examples
  #' x <- mtcars[-1]
  #' y <- unlist(mtcars[1])
  #' select(x, y, randomness=TRUE, G=2)
  
  feature_count <- ncol(X)
  dict.fitness <- new.env()
  initial <- initialize_parents(ncol(X), generation_count=generation_count)
  old_gen <- ranked_models(initial$index, X, y)
  fitness <- old_gen$fitness
  i <- 0   # number of iterations
  while(identical(fitness,rep(fitness[1],length(fitness)))==FALSE){
    #print(old_gen)
    ##### select parents #####
    
    parents <- propotional(old_gen, random = randomness)
    
    ##### crossover and mutation #####
    
    children <- unlist(lapply(parents, breed, C),FALSE, FALSE)
    
    ##### ranked new generation and calculate fitness #####
    
    ranked_new <- ranked_models(children, X, y)
    
    ##### replace k worst old individuals with k new individuals #####
    
    next_gen <- generation_gap(ranked_new, old_gen,G)
    
    ##### let new genration reproudce next offspring ######
    old_gen <- next_gen
    fitness <- old_gen$fitness
    i <- i + 1
  }
  summary <- list(final_gen = old_gen$Index[[1]],fitness = fitness[[1]],num_iteration = i)
  return(summary)
}
