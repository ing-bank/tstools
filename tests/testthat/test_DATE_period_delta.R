
context("period_delta")

test_that("check period_delta with positive integer delta values", {
  function_output <- period_delta(period = 200901, delta = 2)
  expect_equal(function_output, 200903)
  expect_true(is.numeric(function_output))
  function_output <- period_delta(period = 200901, delta = 12)
  expect_equal(function_output, 201001)
  expect_true(is.numeric(function_output))
  function_output <- period_delta(period = 200901, delta = 0)
  expect_equal(function_output, 200901)
  expect_true(is.numeric(function_output))
})


test_that("check period_delta with negative integer delta values", {
  function_output <- period_delta(period = 200901, delta = -2)
  expect_equal(function_output, 200811)
  expect_true(is.numeric(function_output))
  function_output <- period_delta(period = 200901, delta = -14)
  expect_equal(function_output, 200711)
  expect_true(is.numeric(function_output))
  function_output <- period_delta(period = 200901, delta = -0)
  expect_equal(function_output, 200901)
  expect_true(is.numeric(function_output))
})

test_that("check period_delta with non-integer numeric delta values", {
  expect_error(
    period_delta(period = 200901, delta = 0.5)
  )
  expect_error(
    period_delta(period = 200901, delta = -0.5)
  )
  expect_error(
    period_delta(period = 200901, delta = 5.5)
  )
  expect_error(
    period_delta(period = 200901, delta = -5.5)
  )
})

test_that("check period_delta with non-period period values", {
  input_period <- as.Date("2009-01-01")
  expect_error(
    period_delta(period = input_period, delta = 2)
  )
  expect_error(
    period_delta(period = input_period, delta = -2)
  )
  expect_error(
    period_delta(period = "200901", delta = 2)
  )
  expect_error(
    period_delta(period = "200901", delta = -2)
  )
  expect_error(
    period_delta(period = 20090101, delta = 2)
  )
  expect_error(
    period_delta(period = 2009, delta = -2)
  )
})
