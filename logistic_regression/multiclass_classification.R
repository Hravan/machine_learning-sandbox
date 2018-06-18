source('./multiclass_replicate.R')
source('./multiclass_gradient_descent.R')
source('./multi_classify.R')

multiclass_classification <- function(X, y, alpha) {
  # Perform multiclass clasification One-vs-all algorithm
  #
  # Args:
  #   X:     m X n matrix of m observations of n independent variables
  #   y:     vector of m observations of the dependent variable
  #   alpha: learning rate
  #
  # Returns:
  #   function that given a new observation classifies it to one of groups specified in y
  
  categories <- unique(y)
  
  # make length(unique(y)) copies of data
  multiclass_y <- multiclass_replicate(y)
  
  # create length(unique(y)) models of logistic regression
  multiclass_models <- multiclass_gradient_descent(X, multiclass_y, alpha)
  
  
  function(new_obs) {
    categories[multi_classify(new_obs, multiclass_models)]
  }
}


# sketch of the algorithm:
# store all unique values of y
# copy the data into length(y) parts
#   every part has one actual and one dummy y variable
# make length(y) logictic regression models, one for yeach value of y, each model computes
# the probability of new X being in given actual group
# return function being a wrapper over three models in question