
context("get_browser_to_use_for_shiny")

test_that("check get_browser_to_use_for_shiny with valid inputs", {
  function_output <- get_browser_to_use_for_shiny()
  if (is.character(function_output)) {
    expect_is(function_output, "character")
    expect_true(file.exists(function_output))
  } else {
    expect_null(function_output)
  }
  function_output <- get_browser_to_use_for_shiny(browsers = "chrome")
  if (is.character(function_output)) {
    expect_is(function_output, "character")
    expect_true(file.exists(function_output))
  } else {
    expect_null(function_output)
  }
  function_output <- get_browser_to_use_for_shiny(browsers = "firefox")
  if (is.character(function_output)) {
    expect_is(function_output, "character")
    expect_true(file.exists(function_output))
  } else {
    expect_null(function_output)
  }
  # Additional test in case Linux is OS
  if (Sys.info()[["sysname"]] == "Linux") {
    function_output <- get_browser_to_use_for_shiny(browsers = "google-chrome")
    if (is.character(function_output)) {
      expect_is(function_output, "character")
      expect_true(file.exists(function_output))
    } else {
      expect_null(function_output)
    }
  }
})

test_that("check get_browser_to_use_for_shiny with invalid inputs", {
  expect_error(
    get_browser_to_use_for_shiny(browsers = "potato")
  )
})
