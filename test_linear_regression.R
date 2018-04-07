library(testthat)

source('C:/Users/Hravan/Mein_Ordner/Computer Science/Machine_Learning_course/Week_1/Linear_Regression_one_variable/linear_regression.R')

# 1. Hypothesis function

context('Hypothesis')

# Test settings
matr <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = T)
matr2 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = T)

test_that('Hypothesis function for linear regression returns correct values', {
  expect_equal(hypothesis(c(0, 0, 0),matr),
               as.vector(matrix(c(0, 0), nrow = 1)))
  
  expect_equal(hypothesis(c(1, 1, 1), matr),
               as.vector(matrix(c(4, 8), nrow = 1)))
  
  expect_equal(hypothesis(c(0, 0, 0, 0), matr2),
               as.vector(matrix(c(0, 0), nrow = 1)))
  
  expect_equal(hypothesis(c(1, 1, 1, 1), matr2),
               as.vector(matrix(c(7, 16), nrow = 1)))
  
  expect_equal(hypothesis(c(1, 2, 3, 4), matr2),
               as.vector(t(c(1, 2, 3, 4)) %*%  t(cbind(1, matr2))))
})



# 2. Cost function

context('Cost')

# Test settings
matr <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = T)
matr2 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = T)

# expect_equal(cost(matr, ))