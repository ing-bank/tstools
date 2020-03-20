
context("get_date_range")

test_that("check get_date_range output when data frame and one date column is specified", {
  function_output <- get_date_range(
    data = dummy_gasprice,
    date_col = "year_month"
  )
  expect_equal(class(function_output), "list")
  expect_equal(length(function_output), 6)
  expect_equal(function_output$start_ym, 199101)
  expect_equal(function_output$start_year, 1991)
  expect_equal(function_output$start_month, 1)
  expect_equal(function_output$end_ym, 200611)
  expect_equal(function_output$end_year, 2006)
  expect_equal(function_output$end_month, 11)
  function_output <- get_date_range(
    data = dummy_gasprice %>% 
      dplyr::mutate(year_month = date_to_period(year_month)),
    date_col = "year_month"
  )
  expect_equal(class(function_output), "list")
  expect_equal(length(function_output), 6)
  expect_equal(function_output$start_ym, 199101)
  expect_equal(function_output$start_year, 1991)
  expect_equal(function_output$start_month, 1)
  expect_equal(function_output$end_ym, 200611)
  expect_equal(function_output$end_year, 2006)
  expect_equal(function_output$end_month, 11)
})

test_that("check get_date_range when date column is not provided", {
  expect_error(
    get_date_range(
      data = dummy_gasprice
    )
  )
})

test_that("check get_date_range when data is not provided", {
  expect_error(
    get_date_range(
      date_col = "year_month"
    )
  )
})

test_that("check get_date_range when wrong column is chosen for date_col", {
  expect_error(
    get_date_range(
      data = dummy_gasprice,
      date_col = "gasprice"
    )
  )
})

test_that("check get_date_range when the date column is a string", {
  expect_error(
    get_date_range(
      data = dummy_gasprice %>% 
        dplyr::mutate(year_month = as.character(year_month)),
      date_col = "year_month"
    )
  )
})
