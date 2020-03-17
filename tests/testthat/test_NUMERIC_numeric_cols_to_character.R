
context("numeric_cols_to_character")

test_that("check numeric_cols_to_character output with valid input", {
  valid_excludes <- c("" ,"gasprice", "spotprice")
  dummy_gasprice$gasprice <- as.integer(dummy_gasprice$gasprice)
  dummy_gasprice$gemprice <- as.factor(dummy_gasprice$gemprice)
  for (exclude in valid_excludes) {
    function_output <- numeric_cols_to_character(
      data = dummy_gasprice,
      exclude = exclude
    )
    expect_true(is.data.frame(function_output))
    expect_equal(nrow(function_output), 764)
    expect_equal(class(function_output$year_month), "Date")
    expect_equal(class(function_output$state), "character")
    expect_equal(colnames(function_output), c("year_month", "state", "oil_company", "gasprice", "spotprice",
                                              "gemprice"))
    expect_equal(class(function_output$gemprice), "character")

    if (exclude == "gasprice") {
      expect_equal(class(function_output$gasprice), "integer")
      expect_equal(class(function_output$gemprice), "character")
      expect_equal(class(function_output$spotprice), "character")
    }
    if (exclude == "spotprice") {
      expect_equal(class(function_output$gasprice), "character")
      expect_equal(class(function_output$gemprice), "character")
      expect_equal(class(function_output$spotprice), "numeric")
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
  valid_excludes <- c("" ,"numeric_col", "na_col")
  # Run tests across different extended inputs
  for (data in all_data) {
    for (exclude in valid_excludes) {
      function_output <- numeric_cols_to_character(
        data = data,
        exclude = exclude
      )
      expect_true(is.data.frame(function_output))
      expect_equal(nrow(function_output), 2)
      expect_equal(class(function_output$integer_col), "character")
      expect_equal(class(function_output$date_col), "Date")
      expect_equal(class(function_output$factor_col), "character")
      expect_equal(colnames(function_output), c("numeric_col", "integer_col", "date_col", "character_col",
                                                "factor_col", "logical_col", "na_col"))
      if (exclude == "numeric_col") {
        expect_equal(class(function_output$numeric_col), "numeric")
        expect_equal(class(function_output$integer_col), "character")
        expect_equal(class(function_output$na_col), "character")
      }
      if (exclude == "na_col") {
        expect_equal(class(function_output$numeric_col), "character")
        expect_equal(class(function_output$integer_col), "character")
        expect_equal(class(function_output$na_col), "logical")
      }
    }

  }
})


test_that("check get_column_features output with invalid inputs",{
  invalid_data <- list("potato", NULL, NA, c("NA", NA, 42))
  invalid_excludes <- c(41 , NA)
  for (data in invalid_data) {
    for (exclude in invalid_excludes) {
      expect_error(
        numeric_cols_to_character(
          data = data,
          exclude = exclude
        )
      )
    }
  }
})

