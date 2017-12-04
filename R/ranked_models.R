#################################################################################
# Problem to fix:
# Always receive the following error when running calculate_aic:
#   Error in error_func(model) : could not find function "error_func"
# So for this moment, I got rid of the "error_func" argument and replaced 
#   error_func(model) with AIC(model).
# Hopefully we can fix the bug together.
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

calculate_error <- function(index, X, y, error_func=AIC) {
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
  
  X <- X[,index]
  X <- as.data.frame(X)
  y <- as.vector(y)
  
  is_data.frame(X)
  is_vector(y)
  
  stopifnot(nrow(X) == length(y))
  
  X$y <- y
  
  model <- lm(y ~ ., data = X)
  return (error_func(model))
}

ranked_models <- function(chromosome, X, y) {
  ## inputs:
  ##   chromosome      Output of initialize_parents()
  ##   X               Data frame of all features
  ##   y               Output variable
  ##
  ## output: a data frame containing binary list, index list and their
  ##          respective AIC, sorted by AIC in ascending order
  ## 
  ## The smaller the AIC, the better the fit.
  ## 
  ## Examples:
  ##  chromosome <- initialize_parents(5)
  ##  ranked_models(chromosome, df, y)

  index <- chromosome$index
  AIC <- lapply(index, calculate_error, X, y)
  model_AIC <- data.frame(sapply(chromosome, `[`))
  model_AIC$AIC <- unlist(AIC)
  model_AIC <- arrange(model_AIC,AIC)
  return(model_AIC)
}
