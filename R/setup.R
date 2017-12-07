initialize_parents <- function(feature_count, generation_count=2*feature_count) {
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
  if (!is.integer(feature_count)) {
    stop("Feature count is not an integer")
  }
  if (!is.integer(generation_count)) {
    stop("Generation count is not an integer")
  }

  # index_list (returns list of features that are included)
  # binary_list (returns 1 if feature is included, otherwise 0)
  index_list <- list()
  binary_list <- list()

  # creates all the parents in the generation
  while (length(index_list) < generation_count) {

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
