
context("summarize_numeric_value")

test_that("check summarize_numeric_value for valid input values", {
  function_output <- summarize_numeric_value(value = 0)
  expect_equal(function_output, "0 ")
  function_output <- summarize_numeric_value(value = 123123)
  expect_equal(function_output, "123.12 K")
  function_output <- summarize_numeric_value(value = 1512341234)
  expect_equal(function_output, "1.51 B")
  function_output <- summarize_numeric_value(value = 1233.12315)
  expect_equal(function_output, "1.23 K")
  function_output <- summarize_numeric_value(value = -1233.12315)
  expect_equal(function_output, "-1.23 K")
  function_output <- summarize_numeric_value(value = "123124")
  expect_equal(function_output, "123.12 K")
  function_output <- summarize_numeric_value(value = "5231231251351235")
  expect_equal(function_output, "5231.23 T")
  function_output <- summarize_numeric_value(value = "-5231231251351235")
  expect_equal(function_output, "-5231.23 T")

})

test_that("check summarize_numeric_value for valid matrixes (or vector) input values", {
  function_output <- summarize_numeric_value(value = c(0,31434,13414))
  expect_equal(function_output, c("0 ", "31.43 K", "13.41 K"))
  function_output <- summarize_numeric_value(value = c(123123,123.1233,"   13414314 "))
  expect_equal(function_output, c("123.12 K", "123.12 ", "13.41 M"))
  function_output <- summarize_numeric_value(value = c(1.2,31434,13414,1234123451235))
  expect_equal(function_output, c("1.2 ", "31.43 K", "13.41 K", "1.23 T"))
  function_output <- summarize_numeric_value(value = matrix(c(1.2,31434,13414,1234123451235), nrow = 2, ncol = 2, byrow = TRUE))
  expect_equal(function_output, c("1.2 ", "13.41 K", "31.43 K", "1.23 T"))
})


test_that("check summarize_numeric_value for invvalid input values", {
  expect_error(
    summarize_numeric_value(value = c(0.3,"31434 K",13414))
  )
  expect_error(
    summarize_numeric_value(value = c(123.12412,"Berke",3123))
  )
  expect_error(
    summarize_numeric_value(value = c("5N1K",13414))
  )
  expect_error(
    summarize_numeric_value(value = c("0.3  K  ","31434",13414))
  )
})

