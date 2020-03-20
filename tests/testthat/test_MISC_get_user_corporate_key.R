
context("get_user_corporate_key")

test_that("check get_user_corporate_key", {
  user <- get_user_corporate_key()
  expect_true(grepl("^[A-Z]{2}[0-9]{2}[A-Z]{2}$", user))
})
