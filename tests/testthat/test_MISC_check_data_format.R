context("check_data_format")

test_that("check check_data_format output with valid inputs", {
  function_input <- dummy_gasprice %>%
    dplyr::mutate(grouping = paste0("state = ", state, "   &   ", "oil_company = ", oil_company)) %>%
    dplyr::rename("period" = "year_month",
                 "col_of_interest" = "gasprice"
                 ) %>%
    dplyr::select(period, col_of_interest, grouping)

  expect_silent(
    check_data_format(
      data = function_input,
      func_name = "testing",
      req_cols = c("period", "col_of_interest", "grouping")
    )
  )
  expect_silent(
    check_data_format(
      data = function_input %>%
        dplyr::filter(grouping == "state = Indiana   &   oil_company = CompanyA"),
      unique_value_cols = "grouping"
    )
  )

  function_input <- dummy_hierarchical_gasprice %>%
    dplyr::mutate(grouping = paste0("location = ", location, "   &   ", "oil_company = ", oil_company, "currency = ", currency)) %>%
    dplyr::rename("period" = "year_month",
                  "col_of_interest" = "gasprice"
    ) %>%
    dplyr::select(-location, -oil_company, -currency)


  req_cols <- c(
    "period", "col_of_interest", "grouping",
    "spotprice", "gemprice",
    "level_location", "level_oil_company"
  )
  expect_silent(
    check_data_format(
      data = function_input,
      req_cols = req_cols
    )
  )
  expect_silent(
    check_data_format(
      data = function_input %>%
        dplyr::filter(period == "1991-01-31"),
      func_name = "testing",
      req_cols = req_cols,
      unique_value_cols = "period"
    )
  )

  expect_silent(
    check_data_format(
      data = dummy_gasprice,
      req_cols = c("gasprice", "spotprice", "gemprice"),
      unique_value_cols = c()
    )
  )
  expect_silent(
    check_data_format(
      data = dummy_gasprice %>%
        dplyr::filter(state == "New York"),
      req_cols = c(),
      unique_value_cols = "state"
    )
  )
})

test_that("check check_data_format output with invalid inputs", {
  expect_error(
    check_data_format(
      data = list()
    )
  )
  expect_error(
    check_data_format(
      data = "fake_news"
    )
  )
  expect_error(
    check_data_format(
      data = dummy_gasprice,
      func_name = list()
    )
  )
  expect_error(
    check_data_format(
      data = dummy_gasprice,
      req_cols = c("period", "col_of_interest", "grouping")
    )
  )
  expect_error(
    check_data_format(
      data = dummy_gasprice,
      req_cols = c("year_month", "state", "oil_company", "fake_news")
    )
  )
  expect_error(
    check_data_format(
      data = dummy_gasprice %>%
        dplyr::filter(FALSE)
    )
  )
  expect_error(
    check_data_format(
      data = dummy_gasprice,
      unique_value_cols = "state"
    )
  )
})

