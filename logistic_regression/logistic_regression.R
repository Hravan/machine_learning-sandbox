sigmoid <- function(z) {
  # Computes value of the sigmoid function for given real number.
  #
  # Args:
  #   z: double for which we want to compute the value of the sigmoid function
  #
  # Returns:
  #   Output of the sigmoid function.
  e <- exp(1)
  
  1 / (1 + e ^ (-z))
}

hypothesis <- function(theta, X) {
  # Computes hypothesis function for logistic regression
  #
  # Args:
  #   theta: vector of double
  #   X: matrix of observations (with ones added)
  #
  # Returns:
  #   Vector of values of hypothesis for all Xes.
  as.vector(t(theta) %*% t(X))
}
