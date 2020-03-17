
context("get_column_features")

test_that("check get_column_features output with valid input", {
  for (extended in c(TRUE, FALSE)) {
    function_output <- get_column_features(
      data = dummy_gasprice,
      extended = extended
    )
    expect_true(is.data.frame(function_output))
    expect_equal(nrow(function_output), 6)
    expect_equal(class(function_output$name), "character")
    expect_equal(class(function_output$type), "character")
    expect_equal(function_output$name, colnames(dummy_gasprice))
    expect_equal(function_output$type, c("Date", "character", "character",
                                         "numeric", "numeric", "numeric"))
    if (extended == TRUE) {
      expect_equal(ncol(function_output), 4)
      expect_equal(colnames(function_output), c("name", "type", "unique_values", "na_values"))
      expect_equal(class(function_output$unique_values), "integer")
      expect_equal(class(function_output$na_values), "integer")
      expect_equal(function_output$unique_values, c(191, 2, 2, 764, 764, 764))
      expect_equal(function_output$na_values, c(0, 0, 0, 0, 0, 0))
    }
    if (extended == FALSE) {
      expect_equal(ncol(function_output), 2)
      expect_equal(colnames(function_output), c("name", "type"))
    }
  }
})

test_that("check get_column_features output with different valid data formats", {
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
  # Run tests across different extended inputs
  for (data in all_data) {
    for (extended in c(TRUE, FALSE)) {
      function_output <- get_column_features(
        data = data,
        extended = extended
      )
      expect_true(is.data.frame(function_output))
      expect_equal(nrow(function_output), 7)
      expect_equal(class(function_output$name), "character")
      expect_equal(class(function_output$type), "character")
      expect_equal(function_output$name, colnames(data))
      expect_equal(function_output$type, c("numeric", "integer", "Date",
                                           "factor", "factor", "logical", "logical"))
      if (extended == TRUE) {
        expect_equal(ncol(function_output), 4)
        expect_equal(colnames(function_output), c("name", "type", "unique_values", "na_values"))
        expect_equal(class(function_output$unique_values), "integer")
        expect_equal(class(function_output$na_values), "integer")
        expect_equal(function_output$unique_values, c(2, 2, 2, 2, 2, 2, 1))
        expect_equal(function_output$na_values, c(0, 0, 0, 0, 0, 0, 2))
      }
      if (extended == FALSE) {
        expect_equal(ncol(function_output), 2)
        expect_equal(colnames(function_output), c("name", "type"))
      }
    }
  }
})


test_that("check get_column_features output with invalid inputs",{
  invalid_data <- list("potato", matrix(c(2, 4, 3, 1, 5, 7), nrow = 2, ncol = 3, byrow = TRUE),
                       NULL, NA, c("NA", NA, 42))
  for (data in invalid_data) {
    for (extended in c(TRUE, FALSE)) {
      expect_error(
        get_column_features(
          data = data,
          extended = extended
        )
      )
    }
  }
})
