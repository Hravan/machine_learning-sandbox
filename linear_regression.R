# 
# theta - vector of ncol(X) + 1 values
# X - matrix containing independent variables

hypothesis <- function(theta, X) {
  # Computes the value of hypothesis function for linear regression.
  #
  # Args:
  #   theta: vector of coefficients in the equation of linear regression
  #   X: matrix of observations (without the column of ones)
  # Assumption: length(theta) - 1 and ncol(X) are the same
  #
  # Returns:
  #   A vector of hypothesis values given theta and X for all observations in X.
  
  # add a column of ones at the beginning of X
  X <- cbind(1, X)
  
  # return(1)
  # compute hypothesis for all observations in X
  as.vector(t(theta) %*% t(X))
}

cost <- function(theta, X, y) {
  # Computes the cost of using theta as the coefficients for linear regression hypothesis function.
  #
  # Args:
  #   theta: vector of coefficients for linear regression hypothesis
  #   X: matrix of observations without the dependent variable
  #   y: vector of dependent variable
  #
  # Returns:
  #   Cost of using theta in hypothesis function for given X and y.
  m <- nrow(X)
  sum((hypothesis(theta, X) - y) ^ 2) / (2 * m)
}

gradient_descent <- function(theta, X, y, alpha, num_iters) {
  # Computes values of theta that minimize cost function for linear regression
  #
  # Args:
  #   theta: vector of initial coefficients for linear regression hypothesis
  #   X: matrix of observations without the dependent variable
  #   y: vector of dependent variable
  #   alpha: double representing the learning rate
  #   num_iters: integer representing the number of interations of gradient descent do perform
  #
  # Returns:
  #   List containing two vectors of doubles:
  #   Vector of theta that minimze cost function and an additional vector
  #   of cost values after each iteration (cost_values[1] is the value of cost function)
  #   after the first iteration of the algorithm.
  
  # list(theta = c(0, 0), cost_vals = c(0, 0, 0))
  m <- nrow(X)
  X_ones <- cbind(1, X)
  cost_vals <- numeric(num_iters)
  
  for (i in 1:num_iters) {
    r <- res(theta, X, y)
    theta <- theta - alpha / m * as.vector((t(X_ones) %*% r))
    cost_vals[i] <- cost(theta, X, y)
  }
  
  list(theta = theta, cost_vals = cost_vals)
}

res <- function(theta, X, y) {
  # Helper function to compute residuals.
  #
  # Args:
  #   theta: vector of coefficients for a linear regression model
  #   X: matrix of observations without the dependent variable
  #   y: vector of observations of the dependent variable
  #
  # Returns:
  #   Vector of residuals for all observations.
  
  h <- hypothesis(theta, X)
  h - y
}

derivatives <- function(theta, X, y) {
  # Helper function to compute derivatives in gradiend descent algorithm
  #
  # Args:
  #   theta: vector of coefficients for a linear regression model
  #   X: matrix of observations without the dependent variable
  #   y: vector of observations of the dependent variable
  #
  # Returns:
  #   Vector of derivatives for each value of theta.
  X_ones <- cbind(1, X)
  
  r <- matrix(res(theta, X, y), nrow = 1)
  as.vector(t(X_ones) %*% t(r))
}
