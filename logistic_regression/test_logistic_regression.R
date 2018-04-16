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
               hyp_log_reg(theta, X))
})

test_that('Hypothesis for logistic regression is computed properly for two variables X', {
  
  X <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8), nrow = 4)
  X <- cbind(1, X)
  theta <- c(1, 2, 3)
  expected <- sigmoid(t(theta) %*% t(X))
  expect_equal(as.vector(expected),
               hyp_log_reg(theta, X))
})


context('Cost for expected success')

test_that('Cost is close to 0', {
  X <- matrix(c(1, 20, 1, 19), nrow = 2, byrow = T)
  th <- c(0, 1)
  costt <- -log(hyp_log_reg(th, X))
  
  expect_equal(costt,
               cost_success(th, X))
})

test_that('Cost is aproaching Inf', {
  X <- matrix(c(1, -800, 1, -1000), nrow = 2, byrow = T)
  th <- c(0, 1)
  costt <- -log(hyp_log_reg(th, X))
  
  expect_equal(costt,
               cost_success(th, X))
})

test_that('Cost is between 0 and Inf', {
  X <- matrix(c(1, -800, 1, -1000), nrow = 2, byrow = T)
  th <- c(0, 0)
  costt <- -log(hyp_log_reg(th, X))
  
  expect_equal(costt,
               cost_success(th, X))
})


context('Cost for expected failure')

test_that('Cost is aproaching 0', {
  X <- matrix(c(1, -100, 1, -70), nrow = 2, byrow = T)
  th <- c(0, 1)
  costt <- -log(1 - hyp_log_reg(th, X))
  
  expect_equal(costt,
               cost_failure(th, X))
})

test_that('Cost is aproaching Inf', {
  X <- matrix(c(1, 200, 1, 190), nrow = 2, byrow = T)
  th <- c(0, 1)
  costt <- -log(1 - hyp_log_reg(th, X))
  
  expect_equal(costt,
               cost_failure(th, X))
})

test_that('Cost is between 0 and Inf', {
  X <- matrix(c(1, -3, 1, -0.5), nrow = 2, byrow = T)
  th <- c(0, 1)
  costt <- -log(1 - hyp_log_reg(th, X))
  
  expect_equal(costt,
               cost_failure(th, X))
})


context('Cost function - wrapper over separate functions for successes and failures')

test_that('Only positive cases are computed properly', {
  y <- c(1, 1)
  
  X <- matrix(c(1, 20, 1, 19), nrow = 2, byrow = T)
  th <- c(0, 1)
  costt <- 1 / nrow(X) * sum(-log(hyp_log_reg(th, X)))
  
  expect_equal(costt,
               cost(th, X, y))
  
  X <- matrix(c(1, -8, 1, -10), nrow = 2, byrow = T)
  th <- c(0, 1)
  costt <- 1 / nrow(X) * sum(-log(hyp_log_reg(th, X)))
  
  expect_equal(costt,
               cost(th, X, y))
  
  X <- matrix(c(1, -8, 1, -10), nrow = 2, byrow = T)
  th <- c(0, 0)
  costt <- 1 / nrow(X) * sum(-log(hyp_log_reg(th, X)))
  
  expect_equal(costt,
               cost(th, X, y))
})

test_that('Only negative cases are computed properly', {
  y <- c(0, 0)
  
  X <- matrix(c(1, -10, 1, -7), nrow = 2, byrow = T)
  th <- c(0, 1)
  costt <- 1 / nrow(X) * sum(-log(1 - hyp_log_reg(th, X)))
  
  expect_equal(costt,
               cost(th, X, y))
  
  X <- matrix(c(1, 20, 1, 19), nrow = 2, byrow = T)
  th <- c(0, 1)
  costt <- 1 / nrow(X) * sum(-log(1 - hyp_log_reg(th, X)))
  
  expect_equal(costt,
               cost(th, X, y))
  
  X <- matrix(c(1, -3, 1, -0.5), nrow = 2, byrow = T)
  th <- c(0, 1)
  costt <- 1 / nrow(X) * sum(-log(1 - hyp_log_reg(th, X)))
  
  expect_equal(costt,
               cost(th, X, y))
})

test_that('Mixed cases are computed properly', {
  y <- c(1, 1, 0, 0)
  X <- matrix(c(19, -20, 20, -20), nrow = 4)
  X <- cbind(1, X)
  th <- c(1, 1)
  
  expected <- 1 / nrow(X) * sum(-t(y) %*% log(hyp_log_reg(th, X)) - t(1 - y) %*% log(1 - hyp_log_reg(th, X)))
  
  expect_equal(expected,
               cost(th, X, y))
})

context('Residuals')

test_that('Residuals for one independent variable in X are computed properly', {
  X <- matrix(c(2, -2), nrow = 2)
  X <- cbind(1, X)
  y <- c(0, 1)
  th <- c(2, 3)
  r <- hyp_log_reg(th, X) - y
  
  expect_equal(r,
               res(th, X, y, hyp_log_reg))
})

test_that('Residuals for two independent variables in X are computed properly', {
  X <- matrix(c(2, -2, -3, 4), nrow = 2)
  X <- cbind(1, X)
  y <- c(0, 1)
  th <- c(2, 3, 0.5)
  r <- hyp_log_reg(th, X) - y
  
  expect_equal(r,
               res(th, X, y, hyp_log_reg))
})


context('Gradient descent')

test_that('Gradient descent is computed properly', {
  X <- matrix(c(2, -2, -3, 4), nrow = 2)
  X <- cbind(1, X)
  y <- c(0, 1)
  th <- c(2, 3, 0.5)
  alpha <- 0.01
  num_it <- 3
  
  c_vals <- numeric(num_it)
  
  for (i in 1:num_it) {
    c_vals[i] <- cost(th, X, y)
    th <- th - alpha / nrow(X) * t(X) %*% res(th, X, y, hyp_log_reg)
  }
  
  expect_equal(gradient_descent(c(2, 3, 0.5), X, y, alpha, num_it, hyp_log_reg),
               list(theta = th, cost_vals = c_vals))
})
