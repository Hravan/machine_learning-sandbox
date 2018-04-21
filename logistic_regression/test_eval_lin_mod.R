library(testthat)

source('eval_lin_mod.R')

context('Evaluate linear model')

test_that('Predicted value is computed correctly for one given linear model', {
  m1 <- as.matrix(c(2, 3, 4))
  X <- matrix(c(2, 3,
                4, 5,
                7, 8),
              nrow = 3,
              byrow = T)
  X <- cbind(1, X)
  
  result <- matrix(c(2 + 3 * 2 + 4 * 3,
                     2 + 3 * 4 + 4 * 5,
                     2 + 3 * 7 + 4 * 8),
                   nrow = 1)
  
  expect_equal(eval_lin_mod(X, m1),
               result)
  
  m2 <- c(2, 3, 4, 5)
  X <- matrix(c(2, 3, 1,
                4, 5, 6,
                7, 8, 9),
              nrow = 3,
              byrow = T)
  X <- cbind(1, X)
  
  result <- matrix(c(2 + 3 * 2 + 4 * 3 + 5 * 1,
                     2 + 3 * 4 + 4 * 5 + 5 * 6,
                     2 + 3 * 7 + 4 * 8 + 5 * 9),
                   nrow = 1)
  
  expect_equal(eval_lin_mod(X, m2),
               result)
})

test_that('Predicted values are computed properly for many linear models', {
  m1 <- matrix(c(2, 5,
                 3, 6,
                 4, 7),
               nrow = 3,
               byrow = T)
  
  X <- matrix(c(2, 3,
                4, 5,
                7, 8),
              nrow = 3,
              byrow = T)
  X <- cbind(1, X)
  
  result <- matrix(c(2 + 3 * 2 + 4 * 3,
                     2 + 3 * 4 + 4 * 5,
                     2 + 3 * 7 + 4 * 8,
                     5 + 6 * 2 + 7 * 3,
                     5 + 6 * 4 + 7 * 5,
                     5 + 6 * 7 + 7 * 8),
                   nrow = 2,
                   byrow = T)
  
  expect_equal(eval_lin_mod(X, m1),
               result)
  
  m2 <- c(2, 3, 4, 5)
  X <- matrix(c(2, 3, 1,
                4, 5, 6,
                7, 8, 9),
              nrow = 3,
              byrow = T)
  X <- cbind(1, X)
  
  result <- matrix(c(2 + 3 * 2 + 4 * 3 + 5 * 1,
                     2 + 3 * 4 + 4 * 5 + 5 * 6,
                     2 + 3 * 7 + 4 * 8 + 5 * 9),
                   nrow = 1)
  
  expect_equal(eval_lin_mod(X, m2),
               result)
})