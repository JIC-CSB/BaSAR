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

test_that(".BSA.legendre works", {
  x.values <- seq(-1, 1, length=5)

  leg.0 <- .BSA.legendre(0, x.values)
  expect_that(all(leg.0 == 1), is_true())
  
  leg.1 <- .BSA.legendre(1, x.values)
  expect_equal(leg.1, x.values)

  leg.10 <- .BSA.legendre(10, x.values)
  lit.leg.10 <- c(1.000, -0.188, -0.246, -0.188, 1.000)
  expect_equal(leg.10, lit.leg.10, tolerance=0.001)

  leg.5 <- .BSA.legendre(5, x.values)
  leg.6 <- .BSA.legendre(6, x.values)

  expect_equal(numeric(leg.5 %*% leg.6), numeric(0))
})
  
