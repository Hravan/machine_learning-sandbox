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
  # Computes a value of hypothesis function for logistic regression.
  #
  # Args:
  #   theta: vector of coefficients
  #   X: matrix of independent variables (with ones added)
  #
  # Returns:
  #   Vector of values of hypothesis for all Xes.
  as.vector(sigmoid(t(theta) %*% t(X)))
}

cost_success <- function(theta, X) {
  # Computes cost for given values of theta and observations, given that the output of
  # classification should be 1.
  #
  # Args:
  #   theta: vector of coefficients
  #   X:     matrix of independent variables
  #
  # Returns:
  #   double representing the cost of using given parameters for the input of sigmoid function.
  -log(hypothesis(theta, X))
}

cost_failure <- function(theta, X) {
  # Computes cost for given values of theta and observations, given that the output of
  # classification should be 0.
  #
  # Args:
  #   theta: vector of coefficients
  #   X:     matrix of independent variables
  #
  # Returns:
  #   double representing the cost of using given parameters for the input of sigmoid function.
  
  -log(1 - hypothesis(theta, X))
}

cost <- function(theta, X, y) {
  # Wrapper over cost_success and cost_failure.
  #
  # Args:
  #   theta: vector of coefficients
  #   X:     matrix of independent variables
  #   y:     vector of expected successes and failures
  #
  # Returns:
  #   Cost of logistic regression function for given value of theta.
  0
}