library(testthat)

source('lrRegTerm.R')

context('Regularization for logistic regression')

test_that('Punishment is computed properly when all values in theta are 0s', {
  th <- c(0, 0, 0)
  lamb <- 10
  m <- 100
  
  result <- 0.05 * (th[1] ^ 2 + th[2] ^ 2 + th[3] ^ 2)
  
  expect_equal(lrRegTerm(th, lamb, m),
               result)
})

test_that('Punishment is computed properly for nonzero values of theta of length 3', {
  th <- c(1, 2, 3)
  lamb <- 10
  m <- 100
  
  result <- 0.05 * (th[2] ^ 2 + th[3] ^ 2)
  
  expect_equal(lrRegTerm(th, lamb, m),
               result)
})

test_that('Punishment is computed properly for nonzero values of theta of length 4', {
  th <- c(1, 2, 3, 4)
  lamb <- 10
  m <- 100
  
  result <- 0.05 * (th[2] ^ 2 + th[3] ^ 2 + th[4] ^ 2)
  
  expect_equal(lrRegTerm(th, lamb, m),
               result)
})