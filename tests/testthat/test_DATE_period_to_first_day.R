
context("period_to_first_day")

test_that("check period_to_first_day with normal YYYYMM input", {
  function_output <- period_to_first_day(201801)
  expect_equal(class(function_output), "Date")
  expect_true(function_output == as.Date('2018-01-01'))
  function_output <- period_to_first_day(201805)
  expect_equal(class(function_output), "Date")
  expect_true(function_output == as.Date('2018-05-01'))
  function_output <- period_to_first_day(201812)
  expect_equal(class(function_output), "Date")
  expect_true(function_output == as.Date('2018-12-01'))
})

test_that("check period_to_first_day with a vector of normal YYYYMM input", {
  function_output <- period_to_first_day(c(201801, 201805, 201812))
  expect_equal(class(function_output), "Date")
  expect_equal(function_output, as.Date(c('2018-01-01', "2018-05-01", "2018-12-01")))
})

test_that("check period_to_first_day with wrong YYYYMM input", {
  expect_error(
    period_to_first_day(2018)
  )
  expect_error(
    period_to_first_day(20180101)
  )
  expect_error(
    period_to_first_day(000012)
  )
  expect_error(
    period_to_first_day(100000)
  )
  expect_error(
    period_to_first_day(100013)
  )
  expect_error(
    period_to_first_day(c(201801, 20180101, 201805, 2018, 201812))
  )
})

test_that("check period_to_first_day with Date item", {
  expect_error(
    period_to_first_day(as.Date('2018-05-09'))
  )
})

test_that("check period_to_first_day with character item", {
  expect_error(
    period_to_first_day("20031012") 
  )
})
