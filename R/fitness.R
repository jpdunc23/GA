###########################################################################################
# Function: calculate_fitness
#
#' Calculate the fitness of a given model.
#'
#' @param index A vector of indices of selected variables.
#' @param X Data frame containing all available features
#' @param y Response variable
#' @param fit_func Fitness function, with the default being AIC
#' @param family description of the family of distributions to be used.
#' @return A fitness value of the provided model, based on the provided fitness function.
#' @examples
#' fitness <- calculate_fitness(1:3, data.frame(replicate(10,sample(0:1,1000,rep=TRUE))), 1:1000, fit_func=AIC)


calculate_fitness <- function(index, X, y, fit_func, family = gaussian) {
  
  index.str <- paste0(deparse(index), collapse = "")
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
    
    model <- glm(y ~ ., data = X, family = family)
    fitness <- fit_func(model)
    assign(index.str, fitness, envir = dict.fitness)
    return (fitness)
  }
  
}

###########################################################################################
# Function: ranked_models
#
#' Fit the models and rank them by their fitness.
#'
#' @param index A list of indices of selected variables.
#' @param X Data frame of all features
#' @param y Dependent variable
#' @param fit_func Function for fitness measurement. Default is AIC.
#' @param family A description of the family of distributions to be used.
#' @return a data frame containing index list and their respective fitness, sorted by fitness in ascending order
#' @examples
#' X <- mtcars[-1]
#' y <- unlist(mtcars[1])
#' index <-initialize_parents(10,20)$index
#' ranked_models(index, X, y)


ranked_models <- function(index, X, y, fit_func=AIC, family = gaussian) {
  
  fitness_ini <- lapply(index, calculate_fitness, X, y, fit_func, family)
  model_fitness <- data.frame(sapply(list(index), `[`))
  colnames(model_fitness) <- c('Index')
  model_fitness$fitness <- unlist(fitness_ini)
  model_fitness <- arrange(model_fitness,fitness)
  
  return(model_fitness)
}