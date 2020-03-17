
context("period_to_last_day")

test_that("check period_to_last_day with normal YYYYMM input", {
  function_output <- period_to_last_day(201801)
  expect_equal(class(function_output), "Date")
  expect_true(function_output == as.Date('2018-01-31'))
  function_output <- period_to_last_day(201602)
  expect_equal(class(function_output), "Date")
  expect_true(function_output == as.Date('2016-02-29'))
  function_output <- period_to_last_day(201802)
  expect_equal(class(function_output), "Date")
  expect_true(function_output == as.Date('2018-02-28'))
  function_output <- period_to_last_day(201805)
  expect_equal(class(function_output), "Date")
  expect_true(function_output == as.Date('2018-05-31'))
  function_output <- period_to_last_day(201812)
  expect_equal(class(function_output), "Date")
  expect_true(function_output == as.Date('2018-12-31'))
})

test_that("check period_to_last_day with a vector of normal YYYYMM input", {
  function_output <- period_to_last_day(c(201801, 201602, 201802, 201805, 201812))
  expect_equal(class(function_output), "Date")
  expect_equal(function_output, as.Date(c('2018-01-31', "2016-02-29", "2018-02-28", "2018-05-31", "2018-12-31")))
})

test_that("check period_to_last_day with wrong YYYYMM input", {
  expect_error(
    period_to_last_day(2018)
  )
  expect_error(
    period_to_last_day(20180101)
  )
  expect_error(
    period_to_last_day(000012)
  )
  expect_error(
    period_to_last_day(100000)
  )
  expect_error(
    period_to_last_day(100013)
  )
  expect_error(
    period_to_first_day(c(201801, 20180101, 201805, 2018, 201812))
  )
})

test_that("check period_to_last_day with Date item", {
  expect_error(
    period_to_last_day(as.Date('2018-05-09'))
  )
})

test_that("check period_to_last_day with character item", {
  expect_error(
    period_to_last_day("20031012") 
  )
})
