
context("get_feature_with_date_columns")

test_that("check get_feature_with_date_columns output with valid input", {
  # Check with dummy_gasprice data
  function_output <- get_feature_with_date_columns(
    data = dummy_gasprice
  )
  expect_true(is.data.frame(function_output))
  expect_equal(nrow(function_output), 6)
  expect_equal(class(function_output$name), "character")
  expect_equal(class(function_output$type), "character")
  expect_equal(function_output$name, colnames(dummy_gasprice))
  expect_equal(function_output$type, c("Date", "character", "character",
                                       "numeric", "numeric", "numeric"))
  expect_equal(ncol(function_output), 4)
  expect_equal(colnames(function_output), c("name", "type", "unique_values", "na_values"))
  expect_equal(class(function_output$unique_values), "integer")
  expect_equal(class(function_output$na_values), "integer")
  expect_equal(function_output$unique_values, c(191, 2, 2, 764, 764, 764))
  expect_equal(function_output$na_values, c(0, 0, 0, 0, 0, 0))
  # Now test with constructed datasets
  # Initialize columns
  numeric_col <- c(5, 6)
  integer_col <- as.integer(numeric_col)
  date_col <- as.Date(c("2015-05-23", "2019-12-21"))
  character_col <- c("Hello", "La la la")
  factor_col <- as.factor(character_col)
  logical_col <- c(TRUE, FALSE)
  na_col <- c(NA, NA)
  # Initialize data types
  data_frame <- data.frame(
    numeric_col,
    integer_col,
    date_col,
    character_col,
    factor_col,
    logical_col,
    na_col
  )
  tibble <- tibble::as_tibble(data_frame)
  data_table <- data.table::as.data.table(data_frame)
  all_data <- list(data_frame, tibble, data_table)
  # Run tests across different data types
  for (data in all_data) {
    function_output <- get_feature_with_date_columns(
      data = data
    )
    expect_true(is.data.frame(function_output))
    expect_equal(nrow(function_output), 7)
    expect_equal(class(function_output$name), "character")
    expect_equal(class(function_output$type), "character")
    expect_equal(function_output$name, colnames(data))
    expect_equal(function_output$type, c("numeric", "integer", "Date",
                                         "factor", "factor", "logical", "logical"))
    expect_equal(ncol(function_output), 4)
    expect_equal(colnames(function_output), c("name", "type", "unique_values", "na_values"))
    expect_equal(class(function_output$unique_values), "integer")
    expect_equal(class(function_output$na_values), "integer")
    expect_equal(function_output$unique_values, c(2, 2, 2, 2, 2, 2, 1))
    expect_equal(function_output$na_values, c(0, 0, 0, 0, 0, 0, 2))
  }
})

test_that("check get_feature_with_date_columns output with different valid date columns", {
  # Initialize columns
  numeric_col <- c(5, 6)
  integer_col <- as.integer(numeric_col)
  date_col <- as.Date(c("2015-05-23", "2019-12-21"))
  character_col <- c("Hello", "La la la")
  factor_col <- as.factor(character_col)
  logical_col <- c(TRUE, FALSE)
  na_col <- c(NA, NA)
  period_col <- c(201901, 201905)
  numeric_date_col <- c(20190105, 20150815)
  character_date_col <- c("20180512", "20130516")
  factor_date_col <- as.factor(c("20180512", "20130516"))
  # Initialize data types
  data_frame <- data.frame(
    numeric_col,
    integer_col,
    date_col,
    character_col,
    factor_col,
    logical_col,
    na_col,
    period_col,
    numeric_date_col,
    character_date_col,
    factor_date_col
  )
  tibble <- tibble::as_tibble(data_frame)
  data_table <- data.table::as.data.table(data_frame)
  all_data <- list(data_frame, tibble, data_table)
  # Run tests across different data types
  for (data in all_data) {
    function_output <- get_feature_with_date_columns(
      data = data
    )
    expect_true(is.data.frame(function_output))
    expect_equal(nrow(function_output), 11)
    expect_equal(class(function_output$name), "character")
    expect_equal(class(function_output$type), "character")
    expect_equal(function_output$name, colnames(data))
    expect_equal(function_output$type, c("numeric", "integer", "Date",
                                         "factor", "factor", "logical", "logical",
                                         "numeric", "Date", "Date", "Date"))
    expect_equal(ncol(function_output), 4)
    expect_equal(colnames(function_output), c("name", "type", "unique_values", "na_values"))
    expect_equal(class(function_output$unique_values), "integer")
    expect_equal(class(function_output$na_values), "integer")
    expect_equal(function_output$unique_values, c(2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2))
    expect_equal(function_output$na_values, c(0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0))
  }
})


test_that("check get_feature_with_date_columns output with invalid inputs",{
  invalid_data <- list("potato", matrix(c(2, 4, 3, 1, 5, 7), nrow = 2, ncol = 3, byrow = TRUE),
                       NULL, NA, c("NA", NA, 42))
  for (data in invalid_data) {
    expect_error(
      get_feature_with_date_columns(
        data = data
      )
    )
  }
})
