eval_lin_mod <- function(X, models) {
  # Computes a predicted value of the dependend variable given a particular linear model
  #
  # Args:
  #   X:      matrix of observations
  #   model:  matrix of models
  #           every column represents coefficients for one linear model
  #   ASSUMES: ncol(X) equals length(model)
  #
  # Returns:
  #   matrix of predicted values, one row for each model for every observation in X
  t(models) %*% t(X)
}
