library(testthat)
library(purrr)

source('./linear_regression.R')

# 1. Hypothesis function

context('Hypothesis')



test_that('Hypothesis function for linear regression returns correct values', {
  # Test settings
  matr <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = T)
  matr2 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = T)
  
  expect_equal(hypothesis(c(0, 0, 0), matr),
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

test_that('Cost function returns correct values', {
  
  matr <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = T)
  theta <- c(0, 0, 0)
  y <- c(0, 0)
  m <- nrow(matr)
  co <- sum((hypothesis(theta, matr) - y) ^ 2) / (2 * m)
  expect_equal(cost(theta, matr, y),
               co)
  
  y <- c(1, 1)
  co <- sum((hypothesis(theta, matr) - y) ^ 2) / (2 * m)
  expect_equal(cost(theta, matr, y), co)
  
  theta <- c(1, 1, 1, 1)
  matr2 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = T)
  m <- nrow(matr2)
  y <- c(2, 3)
  co <- sum((hypothesis(theta, matr2) - y) ^ 2) / (2 * m)
  expect_equal(cost(theta, matr2, y), co)
})


# 3. Residuals helper for gradient descent

context('Residuals')

test_that('Residuals are computed properly for different number of coefficients
          in linear regression function', {
  
  theta <- c(0, 0)
  X <- matrix(c(1, 1, 1), nrow = 3)
  y <- c(1, 1, 1)
  h <- hypothesis(theta, X)
  result <- h - y
  
  expect_equal(res(theta, X, y),
               result)
  
  theta <- c(0, 0, 0)
  X <- matrix(c(1, 1, 1, 1, 1, 1), nrow = 3)
  y <- c(2, 2, 2)
  h <- hypothesis(theta, X)
  result <- h - y
  
  expect_equal(res(theta, X, y),
               result)
}) # All tests passed

# 4. Derivatives helper for gradient descent

context('Derivatives')


test_that('Derivatives are computed properly for one independent variable case', {
  
  theta <- c(0, 0)
  X <- matrix(c(2, 3, 4), nrow = 3)
  y <- c(1, 1, 1)
  r <- res(theta, X, y)
  
  X_ones <- cbind(1, X)
  
  obs <- numeric(nrow(X))
  drvs <- numeric(length(theta))
  # compute derivatives in a for loop, implementation uses matrices operations
  for (i in 1:length(theta)) { # for every theta
    
    for (j in 1:nrow(X_ones)) { # for every observation
      obs[j] <- res(theta, X[j, ], y[j]) * X_ones[j, i]
    }
    drvs[i] <- sum(obs)
  }
  
  expect_equal(drvs, derivatives(theta, X, y))
})

test_that('Derivatives are computed properly for two independent variables case', {
  
  theta <- c(0, 0, 0)
  X <- matrix(c(2, 3, 4, 5, 6, 7), nrow = 3)
  y <- c(2, 3, 1)
  r <- res(theta, X, y)
  
  X_ones <- cbind(1, X)
  
  obs <- numeric(nrow(X))
  drvs <- numeric(length(theta))
  # compute derivatives in a for loop, implementation uses matrices operations
  for (i in 1:length(theta)) { # for every theta
    
    for (j in 1:nrow(X_ones)) { # for every observation
      obs[j] <- res(theta, matrix(X[j, ], nrow = 1), y[j]) * X_ones[j, i]
    }
    drvs[i] <- sum(obs)
  }
  
  expect_equal(drvs, derivatives(theta, X, y))
})


# 5. Gradient descent

context('Gradient descent')

test_that('Gradient descent and cost are computed properly for one independent variable', {
  
  theta <- c(0, 0)
  X <- matrix(c(1, 2, 3), nrow = 3)
  y <- c(5, 4, 2)
  iter <- 2
  cost_vals <- numeric(iter)
  m <- nrow(X)
  alpha <- 0.01
  
  for (i in 1:iter) {
    cost_vals[i] <- cost(theta, X, y)
    theta <- theta - alpha / m * derivatives(theta, X, y)
  }
  
  expect_equal(gradient_descent(c(0, 0), X, y, alpha, iter),
               list(theta = theta, cost_vals = cost_vals))
})

test_that('Gradient descent and cost are computed properly for two independent variables', {
  
  theta <- c(0, 0, 0)
  X <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
  y <- c(5, 4, 2)
  iter <- 5
  cost_vals <- numeric(iter)
  m <- nrow(X)
  alpha <- 0.01
  
  for (i in 1:iter) {
    cost_vals[i] <- cost(theta, X, y)
    theta <- theta - (alpha / m) * derivatives(theta, X, y)
  }
  
  expect_equal(gradient_descent(c(0, 0, 0), X, y, alpha, iter),
               list(theta = theta, cost_vals = cost_vals))
})


context('Feature scaling - standarization')

test_that('Two features are standarized properly' {
  X <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
  sds <- apply(X, 2, sd)
  means <- apply(X, 2, mean)
  normalized_X <- X
  
  for (i in 1:ncol(X)) {
    normalized_X[, i] <- (X[, i] - means[i]) / sds[i]
  }
})

test_that('Three features are standarized properly' {
  
})