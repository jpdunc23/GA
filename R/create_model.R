library(assertive)

calculate_error <- function(X, y, error_func=AIC) {
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
  
  X <- as.data.frame(X)
  y <- as.vector(y)
  
  is_data.frame(X)
  is_vector(y)
  
  stopifnot(nrow(X) == length(y))
  
  X$y <- y
  
  model <- lm(y ~ ., data = X)
  return (error_func(model))
}
