source('./logistic_regression.R')
source('./eval_lin_mod.R')

multi_classify <- function(X, models) {
  # Compute probability of X belonging to 
  #
  # Args:
  #   X:      matrix of observations for which we want to find groups they belong to
  #   models: matrix of linear models
  #           each model is represented by a columnt of coefficients
  #
  # Returns:
  #   vector of integers representing the predicted group to which observations belong
  raw_results <- sigmoid(eval_lin_mod(X, models))
  
  # apply order function on every column in X
  # the first row contains indices of the predictions with the highest values
  apply(raw_results, 2, order, decreasing = T)[1, ]
}
