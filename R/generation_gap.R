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
  
  next_gen <- dplyr::arrange(next_gen, fitness)
  return(next_gen)
}
