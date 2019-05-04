library(testthat)

source("binomial.R")

context("Test checking functions")

test_that("check success works as expected", {
  x <- 5
  y <- 6
  expect_equal(check_success(x, y), TRUE)
  expect_length(check_success(x, y), 1)
  expect_type(check_success(x, y), 'logical')
})

test_that("check trials works as expected", {
  x <- 5

  expect_equal(check_trials(x), TRUE)
  expect_length(check_trials(x), 1)
  expect_type(check_trials(x), 'logical')
})

test_that("check prob works as expected", {
  x <- .5

  expect_equal(check_prob(x), TRUE)
  expect_length(check_prob(x), 1)
  expect_type(check_prob(x), 'logical')
})
