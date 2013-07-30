setwd('../')
source('BaSAR.R')

context("Testing BaSAR helper functions")

test_that("orthonormalize works", {
  test_m <- matrix(seq(1:4), 2)
  r <- .BSA.orthonormalize(test_m)

  expect_equal(matrix(r$evalues, 1), matrix(cbind(29.866, 0.134), 1),
               tolerance=0.001)
})
  
