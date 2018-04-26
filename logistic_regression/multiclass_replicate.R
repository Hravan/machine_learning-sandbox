multiclass_replicate <- function(y) {
  # Replicate given test set how many unique values in y are times.
  #
  # Args:
  #   y: vector of values of the dependent variable
  #
  # Returns:
  #   list in which every element is a vector of replicated dependent variables.
  #
  cats <- unique(y)
  
  new_ys <- list()
  
  # create a new vector of ys for each category in y
  for (i in 1:length(cats)) {
    new_y <- y
    new_y[y == cats[i]] <- 1
    new_y[y != cats[i]] <- 0
    new_ys[[i]] <- new_y
  }
  new_ys
}
