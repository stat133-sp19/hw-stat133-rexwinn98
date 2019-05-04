library(testthat)

source("binomial.R")


test_that("aux_mean works as expected", {
  p <- .5
  x <- 5
  y <- 6
  expect_equal(aux_mean(x, p), 2.5)
  expect_length(aux_mean(x, p), 1)
  expect_type(aux_mean(x, p), 'double')
})

test_that("aux_variance works as expected", {
  p <- .5
  x <- 5
  y <- 6
  expect_equal(aux_variance(x, p), 1.25)
  expect_length(aux_variance(x, p), 1)
  expect_type(aux_variance(x, p), 'double')
})

test_that("aux_mode works as expected", {
  p <- .5
  x <- 5
  y <- 6
  expect_equal(aux_mode(x, p), 3)
  expect_length(aux_mode(x, p), 1)
  expect_type(aux_mode(x, p), 'integer')
})

test_that("aux_skewness works as expected", {
  p <- .5
  x <- 5
  y <- 6
  expect_equal(aux_skewness(x, p), 0)
  expect_length(aux_skewness(x, p), 1)
  expect_type(aux_skewness(x, p), 'double')
})

test_that("aux_kurtosis works as expected", {
  p <- .5
  x <- 5
  y <- 6
  expect_equal(aux_kurtosis(x, p), -.4)
  expect_length(aux_kurtosis(x, p), 1)
  expect_type(aux_kurtosis(x, p), 'double')
})
