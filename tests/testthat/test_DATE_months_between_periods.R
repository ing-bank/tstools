
context("months_between_periods")

test_that("check months_between_periods for valid start and end periods", {
  function_output <- months_between_periods(start = 200901, end = 201506)
  expect_equal(function_output, 77)
  expect_true(is.numeric(function_output))
  function_output <- months_between_periods(start = 200901, end = 200901)
  expect_equal(function_output,0)
  expect_true(is.numeric(function_output))
  function_output <- months_between_periods(start = 200901, end = 200902)
  expect_equal(function_output, 1)
  expect_true(is.numeric(function_output))
})

test_that("check months_between_periods for vectors of valid start and end periods", {
  function_output <- months_between_periods(start = rep(200901, 10), end = rep(201506, 10))
  expect_equal(function_output, rep(77, 10))
  expect_true(is.numeric(function_output))
  function_output <- months_between_periods(start = rep(200901, 10), end = rep(200901, 10))
  expect_equal(function_output, rep(0, 10))
  expect_true(is.numeric(function_output))
  function_output <- months_between_periods(start = rep(200901, 10), end = rep(200902, 10))
  expect_equal(function_output, rep(1, 10))
  expect_true(is.numeric(function_output))
})

test_that("check months_between_periods for misspecified start and end periods", {
  expect_error(
    months_between_periods(start = 200901, end = 200811)
  )
  expect_error(
    months_between_periods(start = c(200901, 200901, 200901), end = c(200902,200811,200809))
  )
  expect_error(
    months_between_periods(start = "200901", end = 201506)
  )
  input_start <- as.Date("2009-01-01")
  input_end <- as.Date("2015-06-28")
  expect_error(
    months_between_periods(start = input_start, end = input_end)
  )
  expect_error(
    months_between_periods(start = 200901, end = input_end)
  )
  expect_error(
    months_between_periods(start = input_start, end = 201506)
  )
  expect_error(
    months_between_periods(start = 200901, end = 201513)
  )
  expect_error(
    months_between_periods(start = 200913, end = 201506)
  )
  expect_error(
    months_between_periods(start = 200913, end = 201513)
  )
  expect_error(
    months_between_periods(start = 012009, end = 062015)
  )
})
