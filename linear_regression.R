# 
# theta - vector of ncol(X) + 1 values
# X - matrix containing independent variables

hypothesis <- function(theta, X, y = FALSE) {
  # Computes the value of hypothesis function for linear regression.
  #
  # Args:
  #   theta: vector of coefficients in the equation of linear regression (without bias)
  #   X: matrix of observations
  # Assumption: length(theta) and ncol(X) are the same
  #
  # Returns:
  #   A vector of hypothesis values given theta and X for all observations in X.
  
  # add a column of ones at the beginning of X
  X <- cbind(1, X)
  
  # print(t(theta))
  # print(t(X))
  # return(1)
  # compute hypothesis for all observations in X
  as.vector(t(theta) %*% t(X))
}

cost <- function(theta, X, y) {
  # Computes residuals.
  #
  # Args:
  #   theta: vector of 
  #   X: matrix of observations without the dependent variable
  #   y: vector of dependent variable
  #
  # Returns:
  #   Sum of squared residuals (RMSE).
  
  0
}