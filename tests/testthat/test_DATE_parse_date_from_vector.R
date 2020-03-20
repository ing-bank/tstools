
context("parse_date_from_vector")

test_that("check parse_date_from_vector with valid inputs", {
  valid_inputs <- list(
    "2004-10-20", c("2004-1-2", "2004-12-2", "2004-15-2"), 
    c("2015-12-3", "2001-13-2"), "1976-12-31", c("12-5-2010", "13-2-2019"), 
    "1-13-2014", "15-02-2015", c("20040101", "20050201", "20061308", NA), 
    "20102015", c("20150201", "19010215"), "21022015", c(200610, 201512, 200312, "200506"),
    as.Date("2004-10-15")
  )
  for (input in valid_inputs) {
    output <- parse_date_from_vector(input)
    expect_is(output, "Date")
    expect_is(as.numeric(substr(output, start = 1, stop = 4)), "numeric")
    expect_is(as.numeric(substr(output, start = 6, stop = 7)), "numeric")
    expect_is(as.numeric(substr(output, start = 9, stop = 10)), "numeric")
  }
})

test_that("check parse_date_from_vector with invalid inputs", {
  invalid_inputs <- list(
    "I'm a pony", dummy_gasprice, dummy_hierarchical_gasprice, 42, 
    NA, NULL, 200610, c(201208, 201306), "20050101"
  )
  for (input in invalid_inputs) {
    expect_error(
      parse_date_from_vector(input)
    )
  }
})
