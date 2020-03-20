
context("split_grouping_column")

test_that("check split_grouping_column output with group columns still in", {
  grouping_data <- add_grouping_column(
    data = dummy_gasprice,
    group_cols = c("state", "oil_company")
  )
  function_output <- split_grouping_column(data = grouping_data)
  expect_true(is.data.frame(function_output))
  expect_equal(nrow(function_output), 764)
  expect_equal(ncol(function_output), 7)
  expect_equal(colnames(function_output), c("year_month", "state", "oil_company", "gasprice", "spotprice", "gemprice", "grouping"))
  expect_equal(class(function_output$year_month), "Date")
  expect_equal(class(function_output$state), "character")
  expect_equal(class(function_output$oil_company), "character")
  expect_equal(class(function_output$gasprice), "numeric")
  expect_equal(class(function_output$spotprice), "numeric")
  expect_equal(class(function_output$gemprice), "numeric")
  expect_equal(class(function_output$grouping), "character")
  expect_equal(length(unique(function_output$state)), 2)
  expect_equal(length(unique(function_output$oil_company)), 2)
  expect_equal(unique(function_output$state), c("New York", "Indiana"))
  expect_equal(unique(function_output$oil_company), c("CompanyA", "CompanyB"))
})

test_that("check split_grouping_column output with group columns gone", {
  grouping_data <- add_grouping_column(
      data = dummy_gasprice,
      group_cols = c("state", "oil_company")
    ) %>%
    dplyr::select(-state, -oil_company)
  function_output <- split_grouping_column(data = grouping_data)
  expect_true(is.data.frame(function_output))
  expect_equal(nrow(function_output), 764)
  expect_equal(ncol(function_output), 7)
  expect_equal(colnames(function_output), c("year_month", "gasprice", "spotprice", "gemprice", "grouping", "state", "oil_company"))
  expect_equal(class(function_output$year_month), "Date")
  expect_equal(class(function_output$gasprice), "numeric")
  expect_equal(class(function_output$spotprice), "numeric")
  expect_equal(class(function_output$gemprice), "numeric")
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$state), "character")
  expect_equal(class(function_output$oil_company), "character")
  expect_equal(length(unique(function_output$state)), 2)
  expect_equal(length(unique(function_output$oil_company)), 2)
  expect_equal(unique(function_output$state), c("New York", "Indiana"))
  expect_equal(unique(function_output$oil_company), c("CompanyA", "CompanyB"))
})

test_that("check split_grouping_column output with invalid data", {
  expect_error(split_grouping_column(data = "potato"))
})

test_that("check split_grouping_column output without grouping", {
  expect_error(split_grouping_column(data = dummy_gasprice))
})

test_that("check split_grouping_column output with wrong grouping column", {
  grouping_data <- dummy_gasprice %>%
    dplyr::mutate(grouping = "FAKE")
  expect_error(split_grouping_column(data = grouping_data))
})
