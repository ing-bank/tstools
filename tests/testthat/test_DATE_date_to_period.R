
context("date_to_period")

test_that("check date_to_period with normal POSIX.ct items", {
  dummy <- as.POSIXct("2003-10-12")
  function_output <- date_to_period(dummy)
  expect_true(is.numeric(function_output))
  expect_equal(nchar(function_output), 6)
  expect_true(function_output == 200310)
  expect_equal(substr(function_output, 1, 4), "2003")
  expect_equal(substr(function_output, 5, 6), "10")
})

test_that("check date_to_period with Date vector", {
  dummy <- c(as.POSIXct("2003-10-12"), as.POSIXct("2018-05-09"))
  function_output <- date_to_period(dummy)
  expect_true(is.numeric(function_output))
  expect_true(all(nchar(function_output) == 6))
  expect_true(all(function_output %in% c(200310, 201805)))
  expect_equal(substr(function_output, 1, 4), c("2003", "2018"))
  expect_equal(substr(function_output, 5, 6), c("10", "05"))
})

test_that("check date_to_period with dates in string format", {
  dummy <- "2003-10-12"
  expect_error(
    date_to_period(dummy) 
  )
})

test_that("check date_to_period with Date item", {
  dummy <- lubridate::ymd("2003-10-12")
  function_output <- date_to_period(dummy)
  expect_true(is.numeric(function_output))
  expect_equal(nchar(function_output),6)
  expect_true(function_output == 200310)
  expect_equal(substr(function_output, 1, 4), "2003")
  expect_equal(substr(function_output, 5, 6), "10")
})

test_that("check date_to_period with Date vector", {
  dummy <- c(lubridate::ymd("2003-10-12"), lubridate::ymd("2018-05-09"))
  function_output <- date_to_period(dummy)
  expect_true(is.numeric(function_output))
  expect_true(all(nchar(function_output) == 6))
  expect_true(all(function_output %in% c(200310, 201805)))
  expect_equal(substr(function_output, 1, 4), c("2003", "2018"))
  expect_equal(substr(function_output, 5, 6), c("10", "05"))
})

test_that("check date_to_period with numeric item", {
  dummy <- 200310
  expect_error(
    date_to_period(dummy) 
  )
})

test_that("check date_to_period with character item", {
  dummy <- "20031012"
  expect_error(
    date_to_period(dummy) 
  )
})
