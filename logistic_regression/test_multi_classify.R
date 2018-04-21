library(testthat)

source('./multi_classify.R')

context('Multiclass classifier function')

test_that('Groups are predicted correctly according to given models', {
  set.seed(0)
  
  models <- list(c(0.5, 0.2, 0.1), # th0 = 0.5, th1 = 0.2, th2 = 0.1
                 c(2.2, 3.1, 2.1),
                 c(-5, 3.3, 4.2))
  obs <- matrix(c(runif(2, min = -10, max = 10),
                runif(2, min = -10, max = 10),
                runif(2, min = -10, max = 10),
                runif(2, min = -10, max = 10),
                runif(2, min = -10, max = 10)), nrow = 5)
  obs <- cbind(1, obs)
})