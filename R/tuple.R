tuple_single <- function(initial_parents){
  ## input:
  ##   initial parents       list of binary vectos 
  ##
  ## output:
  ##   tuple of parents      list of vectors
  ##
  ## Example:
  ##   initial_parents <- list(c(1,1,1,1,0,0),c(1,0,1,0,0,0),c(0,1,0,1,1,1,1))
  ##   tuple_single(initial_parents)
  parents <- list()
  for(i in 1:length(initial_parents)[1]){
    parents[[i]] <- which(initial_parents[[i]]==1)
  }
  return(parents)
}


tuple_parents <- function(initial_parents){
  ## input:
  ##   initial parents       list of binary vectos, including both parents 
  ##
  ## output:
  ##   tuple of parents      list of vectors
  ##
  ## Example:
  ##   parents1 <- list(c(1,1,1,1,0,0),c(1,0,1,0,0,0),c(0,1,0,1,1,1,1))
  ##   parents2 <- list(c(0,0,1,1,0,0),c(1,1,1,0,0,1),c(0,0,0,1,1,0,1))
  ##   initial_parents <- list(parents1,parents2)
  ##   tuple_parents(initial_parents)
  parents <- list()
  for(i in 1:length(initial_parents)[1]){
    parents[[i]] <- list()
    parents[[i]][[1]] <- which(initial_parents[[i]][[1]]==1)
    parents[[i]][[2]] <- which(initial_parents[[i]][[2]]==1)
  }
  return(parents)
}

                            