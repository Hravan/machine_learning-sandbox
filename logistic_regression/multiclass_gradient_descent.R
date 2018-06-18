source('./logistic_regression.R')

# tests for this function are included in tests for multiclass_classification function
multiclass_gradient_descent <- function(X, rep_y, alpha) {
  # Use gradient desent to find the decision boundaries for each category
  #
  # Args:
  #   X:      array of independent variables
  #   rep_y:  list of vectors of the dependent variables, each element of the list
  #           represents one category (1) against all other categories (0)
  #
  # Returns:
  #   matrix in which each column is a vector of coefficients for a linear model
  
  # create an empty matrix for coefficients for all models
  mat_result <- matrix(nrow = ncol(X), ncol = length(rep_y))
  
  for (i in 1:length(rep_y)) {
    results <- gradient_descent(replicate(n = ncol(X), expr = 0), X, rep_y[[i]], alpha, 10000, hyp_log_reg)
    mat_result[, i] <- results$theta
  }
  
  mat_result
}