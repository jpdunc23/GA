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