#################################################################################
# Problem to fix:
# Always receive the following error when running calculate_aic:
#   Error in error_func(model) : could not find function "error_func"
# So for this moment, I got rid of the "error_func" argument and replaced 
#   error_func(model) with AIC(model).
# Hopefully we can fix the bug together.

X <- mtcars[-1] 
y <- unlist(mtcars[1])
index <-initialize_parents(10,20)$index
AIC <- lapply(index, calculate_aic, X, y, AIC)
model_AIC <- data.frame(sapply(list(index), `[`))
colnames(model_AIC) <- c('Index')
model_AIC$AIC <- unlist(AIC)
model_AIC <- arrange(model_AIC,AIC)

ranked_models(index, X, y)
#################################################################################


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
  ##  aic <- calculate_aic(data.frame(replicate(10,sample(0:1,1000,rep=TRUE))), 1:1000)
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


# parent_str <- deparse(parent)
# memo_AIC <- new.env()
# 
# deparse(c(1,2,3))
# 
# s <- deparse(c(1,2,3))
# 
# memo_AIC$test <- 1
# memo_AIC$test
# 
# memo_AIC$`s`
# memo_AIC
# 
# names(memo_AIC)
# 
# ls(memo_AIC)
# get(s, memo_AIC)
# 
# parent <- c(1,2,3)
# parent_str <- deparse(parent)
# parent_str
# 
# assign(parent_str, 497.1, envir = dict.AIC)
# ls(memo_AIC)
# get(parent_str, envir = dict.AIC)
# 
# "c(2,3,4)" %in% names(memo_AIC)
# 
