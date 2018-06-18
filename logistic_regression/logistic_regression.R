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

hyp_log_reg <- function(theta, X) {
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
  
  -log(hyp_log_reg(theta, X))
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
  
  -log(1 - hyp_log_reg(theta, X))
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
  
  1 / nrow(X) * sum(t(y) %*% cost_success(theta, X) + t(1 - y) %*% cost_failure(theta, X))
}

res <- function(theta, X, y, hyp_fun) {
  # Compute residuals for given hypothesis function for given values of theta
  #
  # Args:
  #   theta:   vectof of coefficients
  #   X:       [m X n] matrix of m observations for n independent variables
  #   y:       vector of m observations for binary (0, 1) dependent variable
  #   hyp_fun: model function used to approximate values in y for given X
  #
  # Returns:
  #   Vector of m values of residuals.
  as.vector(hyp_fun(theta, X) - y)
}

gradient_descent <- function(theta, X, y, alpha, num_iters, hyp_fun) {
  # Perform num_terations of gradient descent algorithm to find values for theta that minimize
  # cost function for given X and y
  #
  # Args:
  #   theta:         vector of initial values of coefficients
  #   X:             matrix [m X n] of m observations for n dependent variables
  #   y:             vector of m observations for independent variable (only zeros and ones)
  #   alpha:         learning rate
  #   num_iters:     number of iterations of the algorithm
  #   hyp_function:  function for which the values of theta are being looked for
  #
  # Returns:
  #   list:
  #     theta: values for theta that minimize the cost function
  #     cost_vals: vector of num_iters cost values, one for every iteration of cost function
  c_vals <- numeric(num_iters)
  for (i in 1:num_iters) {
    c_vals[i] <- cost(theta, X, y)
    theta <- theta - alpha / nrow(X) * t(X) %*% res(theta, X, y, hyp_fun)
  }
  
  list(theta = theta, cost_vals = c_vals)
}
