setwd('../')
source('BaSAR.R')

context("Testing BaSAR helper functions")

test_that("orthonormalize works", {
  test_m <- matrix(seq(1:4), 2)
  r <- .BSA.orthonormalize(test_m)

  expect_equal(matrix(r$evalues, 1), matrix(cbind(29.866, 0.134), 1),
               tolerance=0.001)
})
  
test_that(".BSA.prior works", {
  test_prior <- .BSA.prior(100, 150, 0, 50)

  expect_equal(length(test_prior), 50)
  expect_that(max(test_prior) < 150, is_true())
  expect_that(min(test_prior) > 100, is_true())
})

test_that(".BSA.post works", {
  tpoints <- seq(from=1, to=200, length=200)
  dpoints <- sin(0.5 * tpoints)

  r <- BaSAR.post(dpoints, 6, 600, 100, 0, tpoints)

  # We should have recovered 0.5 as the frequency
  expect_equal(r$stats$omega_mean, 0.5, tolerance=0.01)
})
