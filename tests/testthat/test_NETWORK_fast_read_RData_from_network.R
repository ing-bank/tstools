
context("fast_read_RData_from_network")

dir_for_testing <- tempdir()

test_that("check if fast_read_RData_from_network correctly reads from disk", {
  file_path_for_testing <- file.path(dir_for_testing, "for_testing.RData")
  saveRDS(
    object = dummy_gasprice,
    file = file_path_for_testing
  )
  expect_true(file.exists(file_path_for_testing))
  expect_silent(
    function_output <- fast_read_RData_from_network(
      file_path = file_path_for_testing,
      verbose = F
    )
  )
  expect_true(is.data.frame(function_output))
  expect_equal(nrow(function_output), 764)
  expect_equal(ncol(function_output), 6)
  expect_equal(colnames(function_output), c("year_month", "state", "oil_company", "gasprice", "spotprice", "gemprice"))
  expect_equal(class(function_output$year_month), "Date")
  expect_equal(class(function_output$state), "character")
  expect_equal(class(function_output$oil_company), "character")
  expect_equal(class(function_output$gasprice), "numeric")
  expect_equal(class(function_output$spotprice), "numeric")
  expect_equal(class(function_output$gemprice), "numeric")
  capture.output(
    expect_message(
      function_output <- fast_read_RData_from_network(
        file_path = file_path_for_testing,
        verbose = T
      )
    ),
    file = 'NUL'
  )
})

test_that("check fast_read_RData_from_network output without file_path", {
  expect_error(
    fast_read_RData_from_network()
  )
})

test_that("check fast_read_RData_from_network output with invalid file_path", {
  expect_error(
    fast_read_RData_from_network(
      file_path = "this/is/an/invalid_file_path/sir.RData"
    )
  )
})
