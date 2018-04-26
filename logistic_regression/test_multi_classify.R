library(testthat)

source('./multi_classify.R')

context('Multiclass classifier function')

test_that('Groups are predicted correctly according to given models', {
  
  X11 <- 0.5
  X12 <- 0.5
  X21 <- 1
  X22 <- 6
  X31 <- 4
  X32 <- 4
  X <- matrix(c(X1 = c(X11, X21, X31), X2 = c(X12, X22, X32)), nrow = 3)
  X <- cbind(1, X)
  y <- c(2, 1, 3)
  
  models <- matrix(c( -2, -1,  1,
                       4, -1, -1,
                     -15,  4, 1),
                   nrow = 3)
  
  
  expect_equal(multi_classify(X, models),
               y)
})
