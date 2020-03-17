
context("initialize_ts_forecast_data")

test_that("check initialize_ts_forecast_data output with full specification", {
  function_output <- initialize_ts_forecast_data(
    data = dummy_hierarchical_gasprice,
    date_col = "year_month",
    col_of_interest = "gasprice",
    group_cols = "currency",
    xreg_cols = c("spotprice", "gemprice"),
    hierarchical_cols = c("location", "oil_company")
  )
  expect_true(is.data.frame(function_output))
  expect_equal(nrow(function_output), 10314)
  expect_equal(ncol(function_output), 7)
  expect_equal(colnames(function_output), c(
    "period", "col_of_interest", "grouping", "spotprice", "gemprice", "level_location", "level_oil_company"
  ))
  expect_equal(class(function_output$period), "Date")
  expect_equal(class(function_output$col_of_interest), "numeric")
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$spotprice), "numeric")
  expect_equal(class(function_output$gemprice), "numeric")
  expect_equal(class(function_output$level_location), "numeric")
  expect_equal(class(function_output$level_oil_company), "numeric")
})

test_that("check initialize_ts_forecast_data output with non-hierarchical full specification", {
  function_output <- initialize_ts_forecast_data(
    data = dummy_gasprice,
    date_col = "year_month",
    col_of_interest = "gasprice",
    group_cols = c("state", "oil_company"),
    xreg_cols = c("spotprice", "gemprice")
  )
  expect_true(is.data.frame(function_output))
  expect_equal(nrow(function_output), 764)
  expect_equal(ncol(function_output), 5)
  expect_equal(colnames(function_output), c("period", "col_of_interest", "grouping", "spotprice", "gemprice"))
  expect_equal(class(function_output$period), "Date")
  expect_equal(class(function_output$col_of_interest), "numeric")
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$spotprice), "numeric")
  expect_equal(class(function_output$gemprice), "numeric")
})

test_that("check initialize_ts_forecast_data output without data", {
  expect_error(
    initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    )
  )
})

test_that("check initialize_ts_forecast_data output with period format date_col", {
  function_output <- initialize_ts_forecast_data(
    data = dummy_gasprice %>%
      dplyr::mutate(year_month = date_to_period(year_month)),
    date_col = "year_month",
    col_of_interest = "gasprice",
    group_cols = c("state", "oil_company")
  )
  expect_true(is.data.frame(function_output))
  expect_equal(nrow(function_output), 764)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("period", "col_of_interest", "grouping"))
  expect_equal(class(function_output$period), "Date")
  expect_equal(class(function_output$col_of_interest), "numeric")
  expect_equal(class(function_output$grouping), "character")
})

test_that("check initialize_ts_forecast_data output without or with invalid date_col", {
  expect_error(
    initialize_ts_forecast_data(
      data = dummy_gasprice,
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    )
  )
  expect_error(
    initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "gasprice",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    )
  )
})

test_that("check initialize_ts_forecast_data output without col_of_interest", {
  expect_error(
    initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    )
  )
})

test_that("check initialize_ts_forecast_data output without group_cols", {
  function_output <- initialize_ts_forecast_data(
    data = dummy_gasprice,
    date_col = "year_month",
    col_of_interest = "gasprice",
    xreg_cols = c("spotprice", "gemprice")
  )
  expect_true(is.data.frame(function_output))
  expect_equal(nrow(function_output), 764)
  expect_equal(ncol(function_output), 5)
  expect_equal(colnames(function_output), c("period", "col_of_interest", "grouping", "spotprice", "gemprice"))
  expect_equal(class(function_output$period), "Date")
  expect_equal(class(function_output$col_of_interest), "numeric")
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$spotprice), "numeric")
  expect_equal(class(function_output$gemprice), "numeric")
})

test_that("check initialize_ts_forecast_data output with only one group_cols", {
  function_output <- initialize_ts_forecast_data(
    data = dummy_gasprice,
    date_col = "year_month",
    col_of_interest = "gasprice",
    group_cols = "oil_company",
    xreg_cols = c("spotprice", "gemprice")
  )
  expect_true(is.data.frame(function_output))
  expect_equal(nrow(function_output), 764)
  expect_equal(ncol(function_output), 5)
  expect_equal(colnames(function_output), c("period", "col_of_interest", "grouping", "spotprice", "gemprice"))
  expect_equal(class(function_output$period), "Date")
  expect_equal(class(function_output$col_of_interest), "numeric")
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$spotprice), "numeric")
  expect_equal(class(function_output$gemprice), "numeric")
})

test_that("check initialize_ts_forecast_data output without group_cols", {
  function_output <- initialize_ts_forecast_data(
    data = dummy_gasprice %>%
      dplyr::filter(state == "New York" & oil_company == "CompanyA"),
    date_col = "year_month",
    col_of_interest = "gasprice"
  )
  expect_true(is.data.frame(function_output))
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("period", "col_of_interest", "grouping"))
  expect_equal(class(function_output$period), "Date")
  expect_equal(class(function_output$col_of_interest), "numeric")
  expect_equal(class(function_output$grouping), "character")
  expect_equal(unique(function_output$grouping), "group = all data")
})

test_that("check initialize_ts_forecast_data output without xreg_cols", {
  function_output <- initialize_ts_forecast_data(
    data = dummy_gasprice,
    date_col = "year_month",
    col_of_interest = "gasprice",
    group_cols = c("state", "oil_company")
  )
  expect_true(is.data.frame(function_output))
  expect_equal(nrow(function_output), 764)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("period", "col_of_interest", "grouping"))
  expect_equal(class(function_output$period), "Date")
  expect_equal(class(function_output$col_of_interest), "numeric")
  expect_equal(class(function_output$grouping), "character")
})

test_that("check initialize_ts_forecast_data output with only one xreg_cols", {
  function_output <- initialize_ts_forecast_data(
    data = dummy_gasprice,
    date_col = "year_month",
    col_of_interest = "gasprice",
    group_cols = c("state", "oil_company"),
    xreg_cols = c("spotprice")
  )
  expect_true(is.data.frame(function_output))
  expect_equal(nrow(function_output), 764)
  expect_equal(ncol(function_output), 4)
  expect_equal(colnames(function_output), c("period", "col_of_interest", "grouping", "spotprice"))
  expect_equal(class(function_output$period), "Date")
  expect_equal(class(function_output$col_of_interest), "numeric")
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$spotprice), "numeric")
})

test_that("check initialize_ts_forecast_data output with xreg_cols with more future values", {
  function_input <- dummy_gasprice %>%
    dplyr::mutate(
      gasprice = ifelse(year_month >= as.Date('2006-06-30'), NA, gasprice)
    )
  function_output <- initialize_ts_forecast_data(
    data = function_input,
    date_col = "year_month",
    col_of_interest = "gasprice",
    group_cols = c("state", "oil_company"),
    xreg_cols = c("spotprice", "gemprice")
  )
  expect_true(is.data.frame(function_output))
  expect_equal(nrow(function_output), 764)
  expect_equal(ncol(function_output), 5)
  expect_equal(colnames(function_output), c("period", "col_of_interest", "grouping", "spotprice", "gemprice"))
  expect_equal(class(function_output$period), "Date")
  expect_equal(class(function_output$col_of_interest), "numeric")
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$spotprice), "numeric")
  expect_equal(class(function_output$gemprice), "numeric")
})

test_that("check initialize_ts_forecast_data output with CAPS columns", {
  function_input <- dummy_hierarchical_gasprice
  colnames(function_input) <- toupper(colnames(function_input))
  function_output <- initialize_ts_forecast_data(
    data = function_input,
    date_col = "YEAR_MONTH",
    col_of_interest = "GASPRICE",
    xreg_cols = c("SPOTPRICE", "GEMPRICE"),
    hierarchical_cols = c("LOCATION", "OIL_COMPANY")
  )
  expect_true(is.data.frame(function_output))
  expect_equal(nrow(function_output), 10314)
  expect_equal(ncol(function_output), 7)
  expect_equal(colnames(function_output), c(
    "period", "col_of_interest", "grouping", "spotprice", "gemprice", "level_location", "level_oil_company"
  ))
  expect_equal(class(function_output$period), "Date")
  expect_equal(class(function_output$col_of_interest), "numeric")
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$spotprice), "numeric")
  expect_equal(class(function_output$gemprice), "numeric")
  expect_equal(class(function_output$level_location), "numeric")
  expect_equal(class(function_output$level_oil_company), "numeric")
})

test_that("check initialize_ts_forecast_data output with invalid inputs", {
  expect_error(
    initialize_ts_forecast_data(
      data = "potato"
    )
  )
  expect_error(
    initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "potato"
    )
  )
  expect_error(
    initialize_ts_forecast_data(
      data = dummy_gasprice %>%
        dplyr::mutate(year_month = as.character(year_month)),
      date_col = "year_month"
    )
  )
  expect_error(
    initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "potato"
    )
  )
  expect_error(
    initialize_ts_forecast_data(
      data = dummy_gasprice %>%
        dplyr::mutate(gasprice = as.character(gasprice)),
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    )
  )
  expect_error(
    initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company", "potatos"),
      xreg_cols = c("spotprice", "gemprice")
    )
  )
  expect_error(
    initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice", "potatos")
    )
  )
  expect_error(
    initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice"),
      hierarchical_cols = "year_month"
    )
  )
  expect_error(
    initialize_ts_forecast_data(
      data = dummy_hierarchical_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("location", "oil_company"),
      xreg_cols = c("spotprice", "gemprice"),
      hierarchical_cols = NA
    )
  )
  expect_error(
    initialize_ts_forecast_data(
      data = dummy_hierarchical_gasprice %>%
        dplyr::mutate(
          level_location = 2*level_location
        ),
      date_col = "year_month",
      col_of_interest = "gasprice",
      xreg_cols = c("spotprice", "gemprice"),
      hierarchical_cols = c("location", "oil_company")
    )
  )
})
