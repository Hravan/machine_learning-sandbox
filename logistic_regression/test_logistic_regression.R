library(testthat)

source('./logistic_regression.R')

context('Sigmoid function')

test_that('Sigmoid function returns correct values for a "scalar" input', {
  e <- exp(1)
  
  expect_equal(0.5,
               sigmoid(0))
  
  expect_equal(e / (e + 1),
               sigmoid(1))
  
  expect_equal(e ^ 100 / (e ^ 100 + 1),
               sigmoid(100))
  
  expect_equal(1 / (1 + e ^ 20),
               sigmoid(-20))
})

test_that('Sigmoid function returns correct values in case of vectors', {
  e <- exp(1)
  
  v <- c(0, 1, 100, -20)
  expect_equal(c(0.5, e / (e + 1), e ^ 100 / (e ^ 100 + 1), 1 / (1 + e ^ 20)),
               sigmoid(v))
})

test_that('Sigmoid function returns correct values in case of matrices', {
  matr <- matrix(c(0, 1, 100, -20), nrow = 2)
  expected <- matrix(c(0.5, e / (e + 1), e ^ 100 / (e ^ 100 + 1), 1 / (1 + e ^ 20)), nrow = 2)
  expect_equal(expected,
               sigmoid(matr))
})

context('Hypothesis')

test_that('Hypothesis for logistic regression is computed properly for one variable X', {
  
  X <- matrix(c(1, 2, 3, 4), nrow = 4)
  X <- cbind(1, X)
  theta <- c(0, 0)
  expected <- t(theta) %*% t(X)
  expect_equal(as.vector(expected),
               hypothesis(theta, X))
})

test_that('Hypothesis for logistic regression is computed properly for two variables X', {
  
  X <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8), nrow = 4)
  X <- cbind(1, X)
  theta <- c(1, 2, 3)
  expected <- t(theta) %*% t(X)
  expect_equal(as.vector(expected),
               hypothesis(theta, X))
})
