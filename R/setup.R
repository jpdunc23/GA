library(assertive)

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
  ##  parents <- initialize_parents(10, generation_count = 10, number_features=4)
  
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
    
    # Check for duplicates!
  }
  
  return (list("binary" = binary_list, "index" = index_list))
}
