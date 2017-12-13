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

    if (!is.vector(index)) {
      stop("index is not a vector")
    }

    if (!is.matrix(X) && !is.data.frame(X) ) {
      stop("X is not a matrix or data frame")
    }

    if (!is.vector(y)) {
      stop("Y is not a vector")
    }

    if (!is.function(fit_func)) {
      stop("fit_func is not a function")
    }

    if (!(nrow(X) == length(y))) {
      stop("Number of rows for X and y aren't equal")
    }


    X <- X[,index]
    X <- as.data.frame(X)
    y <- as.vector(y)

    X$y <- y

    model <- glm(y ~ ., data = X, family = family)
    fitness <- tryCatch({
      fit_func(model)
      }, error = function(e) {
        stop("fit_func ran into an error")
      })
    if (!is.numeric(fitness)) {
      stop("fit_func did not return a number")
    }
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


ranked_models <- function(index, X, y, fit_func=AIC, family = gaussian, cluster=NA) {

  # Determine whether or not to use parallelization
  # Apply calculate_fitness to the list of models
  if (all(is.na(cluster))) {
    fitness_ini <- lapply(index, calculate_fitness, X, y, fit_func, family)
  } else {
    fitness_ini <- parallel::parLapply(cluster, index,
                                       calculate_fitness, X, y,
                                       fit_func, family)
  }
  # Turn resulting list to data frame
  model_fitness <- data.frame(sapply(list(index), `[`))
  colnames(model_fitness) <- c('Index')
  model_fitness$fitness <- unlist(fitness_ini)
  # Sort by fitness, in ascending order
  model_fitness <- dplyr::arrange(model_fitness,fitness)

  return(model_fitness)
}

