
context("transform_data_to_ts_object")

test_that("check transform_data_to_ts_object output with full specification", {
  function_output <- dummy_hierarchical_ts_data_w_xregs %>%
    dplyr::filter(grouping == "location = USA   &   oil_company = CompanyC   &   currency = EUR") %>%
    dplyr::select(-level_location, -level_oil_company) %>%
    transform_data_to_ts_object(seasonal_periods = c(12,3))
  expect_equal(class(function_output), c("msts", "ts"))
  expect_equal(colnames(function_output), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 3)
  expect_equal(attr(function_output, "seasonality"), c(12,3))
  expect_equal(attr(function_output, "grouping"), "location = USA   &   oil_company = CompanyC   &   currency = EUR")
  expect_equal(attr(function_output, "xreg_cols"), c("spotprice", "gemprice"))
})

test_that("check transform_data_to_ts_object output with one seasonality period defined (3)", {
  function_output <- dummy_hierarchical_ts_data_w_xregs %>%
    dplyr::filter(grouping == "location = USA   &   oil_company = CompanyC   &   currency = EUR") %>%
    dplyr::select(-level_location, -level_oil_company) %>%
    transform_data_to_ts_object(seasonal_periods = 3)
  expect_equal(class(function_output), c("mts", "ts", "matrix"))
  expect_equal(colnames(function_output), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 3)
  expect_equal(attr(function_output, "seasonality"), 3)
  expect_equal(attr(function_output, "grouping"), "location = USA   &   oil_company = CompanyC   &   currency = EUR")
  expect_equal(attr(function_output, "xreg_cols"), c("spotprice", "gemprice"))
})

test_that("check transform_data_to_ts_object output with one seasonality period defined (12)", {
  function_output <- dummy_hierarchical_ts_data_w_xregs %>%
    dplyr::filter(grouping == "location = USA   &   oil_company = CompanyC   &   currency = EUR") %>%
    dplyr::select(-level_location, -level_oil_company) %>%
    transform_data_to_ts_object(seasonal_periods = 12)
  expect_equal(class(function_output), c("mts", "ts", "matrix"))
  expect_equal(colnames(function_output), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 3)
  expect_equal(attr(function_output, "seasonality"), 12)
  expect_equal(attr(function_output, "grouping"), "location = USA   &   oil_company = CompanyC   &   currency = EUR")
  expect_equal(attr(function_output, "xreg_cols"), c("spotprice", "gemprice"))
})

test_that("check transform_data_to_ts_object output with one seasonality period defined (1)", {
  function_output <- dummy_hierarchical_ts_data_w_xregs %>%
    dplyr::filter(grouping == "location = USA   &   oil_company = CompanyC   &   currency = EUR") %>%
    dplyr::select(-level_location, -level_oil_company) %>%
    transform_data_to_ts_object(seasonal_periods = 1)
  expect_equal(class(function_output), c("mts", "ts", "matrix"))
  expect_equal(colnames(function_output), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 3)
  expect_equal(attr(function_output, "seasonality"), 1)
  expect_equal(attr(function_output, "grouping"), "location = USA   &   oil_company = CompanyC   &   currency = EUR")
  expect_equal(attr(function_output, "xreg_cols"), c("spotprice", "gemprice"))
})

test_that("check transform_data_to_ts_object output with three seasonalities (1,3,12)", {
  function_output <- dummy_hierarchical_ts_data_w_xregs %>%
    dplyr::filter(grouping == "location = USA   &   oil_company = CompanyC   &   currency = EUR") %>%
    dplyr::select(-level_location, -level_oil_company) %>%
    transform_data_to_ts_object(seasonal_periods = c(1,3,12))
  expect_equal(class(function_output), c("msts", "ts"))
  expect_equal(colnames(function_output), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 3)
  expect_equal(attr(function_output, "seasonality"), c(1,3,12))
  expect_equal(attr(function_output, "grouping"), "location = USA   &   oil_company = CompanyC   &   currency = EUR")
  expect_equal(attr(function_output, "xreg_cols"), c("spotprice", "gemprice"))
})

test_that("check transform_data_to_ts_object output with invalid seasonality", {
  function_input <- dummy_hierarchical_ts_data_w_xregs %>%
    dplyr::filter(grouping == "location = USA   &   oil_company = CompanyC   &   currency = EUR") %>%
    dplyr::select(-level_location, -level_oil_company)
  expect_error(
    transform_data_to_ts_object(
      data = function_input,
      seasonal_periods = "string"
    )
  )
  expect_error(
    transform_data_to_ts_object(
      data = function_input,
      seasonal_periods = NULL
    )
  )
})

test_that("check transform_data_to_ts_object output without xreg_cols", {
  function_output <- dummy_hierarchical_ts_data %>%
    dplyr::filter(grouping == "location = USA   &   oil_company = CompanyC   &   currency = EUR") %>%
    dplyr::select(-level_location, -level_oil_company) %>%
    transform_data_to_ts_object(seasonal_periods = c(12,3))
  expect_equal(class(function_output), c("msts", "ts"))
  expect_equal(colnames(function_output), "col_of_interest")
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 1)
  expect_equal(attr(function_output, "seasonality"), c(12,3))
  expect_equal(attr(function_output, "grouping"), "location = USA   &   oil_company = CompanyC   &   currency = EUR")
  expect_equal(attr(function_output, "xreg_cols"), character())
  function_output <- dummy_hierarchical_ts_data %>%
    dplyr::filter(grouping == "location = USA   &   oil_company = CompanyC   &   currency = EUR") %>%
    dplyr::select(-level_location, -level_oil_company) %>%
    transform_data_to_ts_object(seasonal_periods = 12)
  expect_equal(class(function_output), "ts")
  expect_equal(colnames(function_output), "col_of_interest")
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 1)
  expect_equal(attr(function_output, "seasonality"), 12)
  expect_equal(attr(function_output, "grouping"), "location = USA   &   oil_company = CompanyC   &   currency = EUR")
  expect_equal(attr(function_output, "xreg_cols"), character())
})

test_that("check transform_data_to_ts_object output with invalid data", {
  expect_error(
    transform_data_to_ts_object(
      data = "potato"
    )
  )
})

test_that("check transform_data_to_ts_object output without required columns", {
  data <-  dummy_hierarchical_ts_data_w_xregs %>%
    dplyr::filter(grouping == "location = USA   &   oil_company = CompanyC   &   currency = EUR") %>%
    dplyr::select(-level_location, -level_oil_company)
  expect_error(
    transform_data_to_ts_object(
      data = data %>%
        dplyr::select(-period)
    )
  )
  expect_error(
    transform_data_to_ts_object(
      data = data %>%
        dplyr::select(-col_of_interest)
    )
  )
  expect_error(
    transform_data_to_ts_object(
      data = data %>%
        dplyr::select(-grouping)
    )
  )
})

test_that("check transform_data_to_ts_object output with missing period", {
  data <- dummy_hierarchical_ts_data_w_xregs %>%
    dplyr::filter(grouping == "location = USA   &   oil_company = CompanyC   &   currency = EUR") %>%
    dplyr::select(-level_location, -level_oil_company)
  expect_error(
    transform_data_to_ts_object(
      data = data %>%
        dplyr::filter(period != as.Date("2001-09-30"))
    )
  )
})

test_that("check transform_data_to_ts_object output with multiple rows per period", {
  data <- dummy_hierarchical_ts_data_w_xregs %>%
    dplyr::select(-level_location, -level_oil_company)
  expect_error(
    transform_data_to_ts_object(
      data = data
    )
  )
})

test_that("check transform_data_to_ts_object output with non-numeric columns", {
  data <- dummy_hierarchical_ts_data_w_xregs %>%
    dplyr::filter(grouping == "location = USA   &   oil_company = CompanyC   &   currency = EUR") %>%
    dplyr::select(-level_location, -level_oil_company) %>%
    dplyr::mutate(char_col = "charcoal")
  expect_error(
    transform_data_to_ts_object(
      data = data
    )
  )
})
