propotional <- function(models,random = TRUE){
  #' Choose parents from generations propotional to their fitness
  #' @param models: dataframe with indexes and AIC
  #' @param random: if TURE, one parent will be selected randomly
  #' @return The selected parents(list of n lists, each list contains two parents)
  #' @examples
  #' x <- mtcars[-1]
  #' y <- unlist(mtcars[1])
  #' initial_index <- initialize_parents(10,20)$index
  #' rankedmodels <- ranked_models(initial_index,x,y)
  #' propotional(rankedmodels,random = T)
  index <- models$index
  AIC <- unlist(models$AIC)
  n <- nrow(models)
  num_offspring <- ceiling(n/2)
  if(max(AIC)>0&&identical(AIC,rep(AIC[1],length(AIC)))==FALSE){
    AIC <- AIC - max(AIC)
  }
  weight <- AIC / sum(AIC)
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