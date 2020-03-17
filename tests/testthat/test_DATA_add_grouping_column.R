
context("add_grouping_column")

test_that("check add_grouping_column output with full specification", {
  function_output <- add_grouping_column(
    data = dummy_gasprice, 
    group_cols = c("state", "oil_company")
  )
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
  expect_equal(length(unique(function_output$grouping)), 4)
  grouping_values <- c(
    "state = New York   &   oil_company = CompanyA", 
    "state = New York   &   oil_company = CompanyB", 
    "state = Indiana   &   oil_company = CompanyA", 
    "state = Indiana   &   oil_company = CompanyB"
  )
  expect_equal(unique(function_output$grouping), grouping_values)
})

test_that("check add_grouping_column output with only one group_cols", {
  function_output <- add_grouping_column(
    data = dummy_gasprice, 
    group_cols = "state"
  )
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
  expect_equal(length(unique(function_output$grouping)), 2)
  grouping_values <- c("state = New York", "state = Indiana")
  expect_equal(unique(function_output$grouping), grouping_values)
})

test_that("check add_grouping_column output with wrong data", {
  expect_error(add_grouping_column(data = "potato"))
})

test_that("check add_grouping_column output without group_cols", {
  expect_error(add_grouping_column(data = dummy_gasprice))
})

test_that("check add_grouping_column output without data", {
  expect_error(add_grouping_column(group_cols = c("state", "oil_company")))
})

test_that("check add_grouping_column output with non-existing columns", {
  expect_error(add_grouping_column(data = dummy_gasprice, group_cols = c("state", "oil_company", "non_existing")))
})
