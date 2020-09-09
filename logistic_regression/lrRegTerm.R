lrRegTerm <- function(theta, lambda, m) {
  # Helper function to compute the additional regularization term for logistic regression
  #
  # Args:
  #   theta:  vector of coefficients used in hypothesis function for logistic regression
  #   lambda: regularization parameter
  #   m:      number of samples
  #
  # Returns:
  #   (numeric) the regularization term.
  
  squared_sum_theta <- Reduce(sum,
                              Map(function(x) x^2, theta[-1]))
  
  lambda / (2 * m) * squared_sum_theta
}
