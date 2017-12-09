###########################################################################################
# Function: tournament
#
#' Choose parents using tournament selection.
#' Each time, randomly pick k models from the previous generation and
#' the best-performed model will be selected as the parent.
#' Repeat this process until all slots for parents are filled.
#' @param models dataframe with models (specified by the index of selected variables)
#' and their respective fitness value. Object returned by ranked_models().
#' @param k size of each round of selection. Must be an integer smaller than generation size.
#' @return The selected parents(list of ceiling(nrow(models)/2) lists, each list contains two parents)
#' @examples
#' x <- mtcars[-1]
#' y <- unlist(mtcars[1])
#' initial_index <- initialize_parents(10,20)$index
#' rankedmodels <- ranked_models(initial_index,x,y)
#' tournament(rankedmodels,k = 2)

tournament <- function(models, k){

  if (k %% 1!=0) {
    stop("Tournament size k is not an integer")
  }
  if (k>nrow(models)) {
    stop("K cannot be greater than generation size P")
  }

  if (class(models) != "data.frame") {
    stop("input models must be a data frame")
  }
  if (ncol(models) != 2) {
    stop("input models have incorrect number of columns; should be 2")
  }
  if (length(which(is.na(models$Index))) != 0) {
    stop("NAs in the index column")
  }
  if (length(which(is.na(models$fitness))) != 0) {
    stop("NAs in the fitness column")
  }
  if (is.numeric(models$fitness) != TRUE) {
    stop("fitness is not numeric values")
  }

  num_offspring <- ceiling(nrow(models)/2)
  M <- t(matrix(rep(1:nrow(models)),nrow=nrow(models), ncol = 2*num_offspring))
  sample <- t(apply(M, 1, function(x) sample.int(length(x), size = k, replace = FALSE)))
  ind <- apply(sample, 1, function(x) return(which.min(models$fitness[x])))
  ind <- cbind(sample, ind)
  parent_ind <- apply(ind, 1, function(x) return(x[x[k+1]]))

  split_index <- sample(parent_ind, num_offspring, replace=FALSE)
  father <- models$Index[split_index]
  mother <- models$Index[-split_index]
  parents <- list()

  for(i in 1:num_offspring) {
    parents[[i]] <- list(father[[i]],mother[[i]])
  }
  return(parents)
}


###########################################################################################
# Function: proportional
#
#' Choose parents from generations propotional to their fitness
#' @param models: dataframe with models (specified by the index of selected variables)
#' and their respective fitness value
#' @param random: if TURE, one parent will be selected randomly
#' @return The selected parents(list of n lists, each list contains two parents)
#' @examples
#' x <- mtcars[-1]
#' y <- unlist(mtcars[1])
#' initial_index <- initialize_parents(10,20)$index
#' rankedmodels <- ranked_models(initial_index,x,y)
#' propotional(rankedmodels,random = T)

propotional <- function(models,random = TRUE){

  if (class(models) != "data.frame") {
    stop("input models must be a data frame")
  }
  if (ncol(models) != 2) {
    stop("input models have incorrect number of columns; should be 2")
  }
  if (length(which(is.na(models$Index))) != 0) {
    stop("NAs in the index column")
  }
  if (length(which(is.na(models$fitness))) != 0) {
    stop("NAs in the fitness column")
  }
  if (is.numeric(models$fitness) != TRUE) {
    stop("fitness is not numeric values")
  }

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
