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
  e <- exp(1)
  
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
  expected <- sigmoid(t(theta) %*% t(X))
  expect_equal(as.vector(expected),
               hypothesis(theta, X))
})

test_that('Hypothesis for logistic regression is computed properly for two variables X', {
  
  X <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8), nrow = 4)
  X <- cbind(1, X)
  theta <- c(1, 2, 3)
  expected <- sigmoid(t(theta) %*% t(X))
  expect_equal(as.vector(expected),
               hypothesis(theta, X))
})


context('Cost for expected success')

test_that('Cost is close to 0', {
  X <- matrix(c(1, 20, 1, 19), nrow = 2, byrow = T)
  th <- c(0, 1)
  costt <- -log(hypothesis(th, X))
  
  expect_equal(costt,
               cost_success(th, X))
})

test_that('Cost is aproaching Inf', {
  X <- matrix(c(1, -800, 1, -1000), nrow = 2, byrow = T)
  th <- c(0, 1)
  costt <- -log(hypothesis(th, X))
  
  expect_equal(costt,
               cost_success(th, X))
})

test_that('Cost is between 0 and Inf', {
  X <- matrix(c(1, -800, 1, -1000), nrow = 2, byrow = T)
  th <- c(0, 0)
  costt <- -log(hypothesis(th, X))
  
  expect_equal(costt,
               cost_success(th, X))
})


context('Cost for expected failure')

test_that('Cost is aproaching 0', {
  X <- matrix(c(1, -100, 1, -70), nrow = 2, byrow = T)
  th <- c(0, 1)
  costt <- -log(1 - hypothesis(th, X))
  
  expect_equal(costt,
               cost_failure(th, X))
})

test_that('Cost is aproaching Inf', {
  X <- matrix(c(1, 200, 1, 190), nrow = 2, byrow = T)
  th <- c(0, 1)
  costt <- -log(1 - hypothesis(th, X))
  
  expect_equal(costt,
               cost_failure(th, X))
})

test_that('Cost is between 0 and Inf', {
  X <- matrix(c(1, -3, 1, -0.5), nrow = 2, byrow = T)
  th <- c(0, 1)
  costt <- -log(1 - hypothesis(th, X))
  
  expect_equal(costt,
               cost_failure(th, X))
})

# test_that('Cost function returns cost between 0 and Inf', {
#   X <- matrix(c(2, 0.4), nrow = 2)
#   X <- cbind(1, X)
#   y <- c(1, 0)
#   th <- c(0, 1)
#   h <- hypothesis(th, X)
#   
#   co <- 1 / nrow(X) * (-t(y) %*% log(h) - t(1 - y) %*% log(1 - h))
#   expect_equal(co,
#                cost(th, X, y))
#   
# })
# 
# test_that('Cost function returns cost equal close to 0', {
#   X <- matrix(c(20, 19), nrow = 2)
#   X <- cbind(1, X)
#   y <- c(1, 1)
#   th <- c(0, 1)
#   h <- hypothesis(th, X)
#   
#   co <- 1 / nrow(X) * (-t(y) %*% log(h) - t(1 - y) %*% log(1 - h))
#   expect_equal(co,
#                cost(th, X, y))
#   
# })
# 
# test_that('Cost function returns cost equal Inf', {
#   X <- matrix(c(-100, -200), nrow = 2)
#   X <- cbind(1, X)
#   y <- c(1, 1)
#   th <- c(0, 1)
#   h <- hypothesis(th, X)
#   
#   co <- 1 / nrow(X) * (-t(y) %*% log(h) - t(1 - y) %*% log(1 - h))
#   expect_equal(co,
#                cost(th, X, y))
#   
# })
# 
# test_that('Cost is computed properly for two independent variables', {
#   
# })