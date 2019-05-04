
library(testthat)

source("binomial.R")


context("tests for the binomial functions")

test_that("bin_choose works as expected", {
  p <- .5
  x <- 5
  y <- 6
  expect_equal(bin_choose(y, x), 6)
  expect_length(bin_choose(y, x), 1)
  expect_type(bin_choose(y, x), 'double')
})

test_that("bin_probability works as expected", {
  p <- .5
  x <- 5
  y <- 6
  expect_equal(bin_probability(x, y, p), 0.09375)
  expect_length(bin_probability(x, y, p), 1)
  expect_type(bin_probability(x, y, p), "double")
})

test_that("bin_distribution works as expected", {
  p <- .5
  x <- 5
  y <- 6
  expect_length(bin_distribution(y, p)[[1]], y + 1)
  expect_length(bin_distribution(y, p), 2)
  expect_type(bin_distribution(y, p), "list")
})

test_that("bin_distribution works as expected", {
  p <- .5
  x <- 5
  y <- 6
  expect_length(bin_cumulative(y, p)[[1]], y + 1)
  expect_length(bin_cumulative(y, p), 3)
  expect_type(bin_distribution(y, p), "list")
})


