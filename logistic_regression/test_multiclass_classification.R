library(testthat)

source('multiclass_classification.R')


context('Multiclass classification')

test_that('Multiclass classification classifies correctly', {
  # Random datapoints with 
  X11 <- runif(100, min = 0, max = 2)
  X12 <- runif(100, min = 0, max = 2)
  X21 <- runif(100, min = 3, max = 5)
  X22 <- runif(100, min = 3, max = 5)
  X31 <- runif(100, min = 0, max = 2)
  X32 <- runif(100, min = 5, max = 7)
  X <- matrix(c(X1 = c(X11, X21, X31), X2 = c(X12, X22, X32)), nrow = 300)
  X <- cbind(1, X)
  y <- as.matrix(c(replicate(100, 1), replicate(100, 2), replicate(100, 3)))
  
  # Observations which should be classified as 1, 2 and 3
  new_X1 <- matrix(c(runif(1, min = 0, max = 2), runif(1, min = 0, max = 2)), nrow = 1)
  new_X1 <- cbind(1, new_X1)
  new_X2 <- matrix(c(runif(1, min = 3, max = 5), runif(1, min = 3, max = 5)), nrow = 1)
  new_X2 <- cbind(1, new_X2)
  new_X3 <- matrix(c(runif(1, min = 0, max = 2), runif(1, min = 5, max = 7)), nrow = 1)
  new_X3 <- cbind(1, new_X3)
  
  pred <- multiclass_classification(X, y, alpha = 0.01)
  
  expect_equal(pred(new_X1),
               1)
  expect_equal(pred(new_X2),
               2)
  expect_equal(pred(new_X3),
               3)
})


# # Prepared data
# X11 <- runif(100, min = 0, max = 2)
# X12 <- runif(100, min = 0, max = 2)
# X21 <- runif(100, min = 3, max = 5)
# X22 <- runif(100, min = 3, max = 5)
# X31 <- runif(100, min = 0, max = 2)
# X32 <- runif(100, min = 5, max = 7)
# X <- matrix(c(X1 = c(X11, X21, X31), X2 = c(X12, X22, X32)), nrow = 300)
# y <- as.matrix(c(replicate(100, 1), replicate(100, 2), replicate(100, 3)))
# df <- data.frame(X1 = X[, 1], X2 = X[, 2], y = y)
# ggplot(df, aes(x = X1, y = X2, col = as.factor(y))) +
#   geom_point() +
#   geom_abline(slope = 1, intercept = 2) +
#   geom_abline(slope = -1, intercept = 4) +
#   geom_abline(slope = -4, intercept = 15)
