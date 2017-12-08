###########################################################################################
# Function: initialize_parents
#
#' Generate the first generation of models.
#'
#' @param feature_count Number of features given
#' @param P Number of parents in the generation
#' @return A list of selected models, with each specified by the indices of selected columns.
#' @examples
#' parents <- initialize_parents(10, P = 10, number_features=4)

initialize_parents <- function(feature_count, P=2*feature_count) {

  # Sanitize input, ensure everything is an integer
  feature_count <- as.integer(feature_count)
  P <- as.integer(P)

  # Ensure input is sanitized correctly
  if (!is.integer(feature_count)) {
    stop("Feature count is not an integer")
  }
  if (!is.integer(P)) {
    stop("Generation count is not an integer")
  }

  # index_list (returns list of features that are included)
  # binary_list (returns 1 if feature is included, otherwise 0)
  index_list <- list()
  binary_list <- list()

  # creates all the parents in the generation
  while (length(index_list) < P) {

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


###########################################################################################
# Function: breed
#
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

  for(i in 1:num_offspring){
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

###########################################################################################
# Function: generation_gap
#
#' Replace a proportion G of the old generation with their offspring.
#'
#' @param old_gen Output of ranked_models() of the previous generation.
#'   A data frame.
#' @param children Output of ranked_models() of the children of the
#'   previous generation. A data frame.
#' @param G Proportion of old_gen to replace with children.
#' @return The next generation, in descending order of fitness
#'   (most fit first). A data frame.

generation_gap <- function(old_gen, children, G = 1) {

    n_old <- nrow(old_gen)
    ## tournament selection can result
    ## in more children than parents
    n_child <- min(nrow(children), n_old)

    ## at least one of the old_gen must be replaced
    n_remove <- max(floor(G * n_old), 1)

    ## how many to keep from the old generation
    ## including any size gap between old gen and
    ## children due to duplicates in the children
    ## at most n_old - 1 should be kept
    n_keep <- min(n_old - n_remove + (n_old - n_child),
                  n_old - 1)

    if (n_keep > 0) {
        ## keep top n_keep from old_gen and fill
        ## remaining space with best children
        next_gen <- rbind(old_gen[1:n_keep, ],
                          children[1:(n_old - n_keep), ])
        uniq <- !duplicated(next_gen$Index)
        next_gen <- next_gen[uniq, ]
    } else {
        next_gen <- children
    }

    n_next <- nrow(next_gen)
    if (n_next < n_old) {
        i <- n_keep + 1
        while (i <= n_old && n_next < n_old) {
            ## check if old_gen[i, ] is not already
            ## in next_gen and if so, add it to next_gen
            new = is.na(
                Position(function(x) {
                    identical(x, old_gen$Index[[i]])
                }, next_gen$Index)
            )
            if (new) next_gen <- rbind(next_gen, old_gen[i, ])

            ## update index and next_gen size
            i <- i + 1
            n_next <- nrow(next_gen)
        }
    }

    next_gen <- next_gen %>% arrange(fitness)
    return(next_gen)
}

###########################################################################################
# Function: select
#
#' Ranked each model by its fitness,
#' Choose parents from generations propotional to their fitness
#' Do crossover and mutation
#' Replace a proportion G of the worst old individuals by best
#'   new individuals
#' @param X: dataframe containing vairables in the model
#' @param y: vector targeted variable
#' @param C The length of chromosomes, i.e. the maximum number of
#'   possible predictors.
#' @param family: a description of the error distribution and link function to be used in gm.
#' @param selection: selection mechanism. Can be either "proportional" or "tournament".
#' @param K: size of each round of selection when using tournament selection.
#'   Must be an integer smaller than generation size.
#' @param randomness: if TURE, one parent will be selected randomly
#' @param P: population size
#' @param G: proportion of worst-performing parents the user wishes to replace by best offspring
#' @param n_splits: number of crossover points to use in breeding
#' @param op: An optional, user-specified genetic operator function
#'   to carry out the breeding.
#' @param fit_func: Function for fitness measurement. Default is AIC.
#' @param max_iter: how many iterations to run before stopping
#' @return The best individual seen over all iterations.
#' @examples
#' x <- mtcars[-1]
#' y <- unlist(mtcars[1])
#' select(x, y, selection = "tournament", K = 5, randomness=TRUE, G=0.8)
#' @export

select <- function(X, y, C = ncol(X), family = gaussian,
                   selection = "tournament", K = 2,
                   randomness = TRUE, P = 2 * ncol(X),
                   G = 1/P, n_splits = 2, op = NULL,
                   fit_func = AIC, max_iter = 100, ...) {
  library(plyr)
  library(dplyr)
  library(assertive)


  feature_count <- ncol(X)
  dict.fitness <<- new.env()
  initial <- initialize_parents(ncol(X), P)
  old_gen <- ranked_models(initial$index, X, y, fit_func)
  fitness <- old_gen$fitness

  best <- c() ## best seen so far
  best_i <- 0 ## iteration when it was seen
  best_fit <- Inf ## fitness of best so far

  i <- 0   # number of iterations
  while(i < max_iter) {

    ##### select parents #####

    if (selection == "proportional"){
      parents <- propotional(old_gen, random = randomness)
    } else {
      parents <- tournament(old_gen, k=K)
    }

    ##### crossover and mutation #####

    children <- unique(unlist(lapply(parents, breed, C, n_splits, op),
                              FALSE, FALSE))

    ##### ranked new generation and calculate fitness #####

    ranked_new <- ranked_models(children, X, y, fit_func)

    ##### replace k worst old individuals with k new individuals #####

    next_gen <- generation_gap(old_gen, ranked_new, G)

    ## update our best so far if necessary
    if (next_gen$fitness[1] < best_fit) {
        best_fit <- next_gen$fitness[1]
        best <- next_gen$Index[[1]]
        best_i <- i + 1
    }

    ##### let new genration reproudce next offspring ######
    old_gen <- next_gen
    fitness <- old_gen$fitness
    i <- i + 1
  }

  summary <- list(survivor = best, fitness = best_fit,
                    num_iteration = i, first_seen = best_i)
  return(summary)
}

#### example
set.seed(1)
n <- 500
C <- 20
X <- matrix(rnorm(n * C), nrow = n)
beta <- c(88, 0.1, 123, 4563, 1.23, 20)
y <- X[ ,1:6] %*% beta
colnames(X) <- c(paste("real", 1:6, sep = ""),
                 paste("noi", 1:14, sep = ""))
# system.time(
#     o1 <- select(X, y, nsplits = 3, max_iter = 200)
# )
# o1
# system.time(
#     o2 <- select(X, y, selection = "proportional", n_splits = 3)
# )
# o2
