
context("period_to_quarter")

test_that("check period_to_quarter with normal YYYYMM input", {
  function_output <- period_to_quarter(201801)
  expect_equal(class(function_output), "numeric")
  expect_true(function_output == 1)
  function_output <- period_to_quarter(201602)
  expect_equal(class(function_output), "numeric")
  expect_true(function_output == 1)
  function_output <- period_to_quarter(201704)
  expect_equal(class(function_output), "numeric")
  expect_true(function_output == 2)
  function_output <- period_to_quarter(201807)
  expect_equal(class(function_output), "numeric")
  expect_true(function_output == 3)
  function_output <- period_to_quarter(201812)
  expect_equal(class(function_output), "numeric")
  expect_true(function_output == 4)
})

test_that("check period_to_quarter with a vector of normal YYYYMM input", {
  function_output <- period_to_quarter(201801:201812)
  expect_equal(class(function_output), "numeric")
  expect_equal(function_output, c(rep(1,3), rep(2,3), rep(3,3), rep(4,3)))
})

test_that("check period_to_quarter with wrong YYYYMM input", {
  expect_error(
    period_to_quarter(2018)
  )
  expect_error(
    period_to_quarter(20180101)
  )
  expect_error(
    period_to_quarter(000012)
  )
  expect_error(
    period_to_quarter(100000)
  )
  expect_error(
    period_to_quarter(100013)
  )
  expect_error(
    period_to_first_day(c(201801, 20180101, 201805, 2018, 201812))
  )
})

test_that("check period_to_quarter with Date item", {
  expect_error(
    period_to_quarter(as.Date('2018-05-09'))
  )
})

test_that("check period_to_quarter with character item", {
  expect_error(
    period_to_quarter("20031012") 
  )
})
