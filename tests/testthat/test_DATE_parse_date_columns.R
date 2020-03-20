context("parse_date_columns")

test_that("check parse_date_columns with valid inputs", {
  # All cases where dataset has well defined date column
  well_defined_data <- list(dummy_gasprice, dummy_hierarchical_gasprice)
  for (data in well_defined_data) {
    function_output <- parse_date_columns(data)
    expect_equal(function_output, data)
  }
  # Create dataset with potential date columns
  function_input <- dummy_gasprice %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      period_col = sample(201601:201612, 1, replace = T),
      eu_date_col = sample(c("05-12-2015", "28-04-2019", "20-02-1990"), 1, replace = T),
      us_date_col = sample(c("12-05-2015", "04-28-2019", "02-20-1990"), 1, replace = T),
      string_date_col = sample(c("05122015", "28042019", "20021990"), 1, replace = T),
      numeric_date_col = sample(c(05122015, 28042019, 20021990), 1, replace = T)
    ) %>%
    dplyr::ungroup()
  function_output <- parse_date_columns(function_input)
  expect_equal(colnames(function_input), colnames(function_output))
  expect_equal(function_input$period_col, function_output$period_col)
  expect_equal(class(function_output$eu_date_col), "Date")
  expect_equal(class(function_output$us_date_col), "Date")
  expect_equal(class(function_output$string_date_col), "Date")
  expect_equal(class(function_output$numeric_date_col), "Date")
  expect_equal(sort(unique(function_output$eu_date_col)), sort(unique(function_output$us_date_col)))
  expect_equal(sort(unique(function_output$us_date_col)), sort(unique(function_output$string_date_col)))
  expect_equal(sort(unique(function_output$string_date_col)), sort(unique(function_output$numeric_date_col)))
  expect_equal(sort(unique(function_output$eu_date_col)), sort(unique(function_output$string_date_col)))
  expect_equal(sort(unique(function_output$eu_date_col)), sort(unique(function_output$numeric_date_col)))
  expect_equal(function_input[,1:7], function_output[,1:7])
})

test_that("check parse_date_columns with invalid inputs", {
  invalid_inputs <- list("NA", c(NA, NULL, 42), list(), 201601:201612, tibble::tibble())
  for (data in invalid_inputs) {
    expect_error(
      parse_date_columns(
        data = data
      )
    )
  }
})
