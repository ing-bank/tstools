
context("unlist_if_required")

test_that("check unlist_if_required function with one item list", {
  function_input <- list(data = dummy_gasprice)
  function_output <- unlist_if_required(function_input)
  expect_equal(function_output, dummy_gasprice)
  expect_true(is.data.frame(function_output))
  function_input <- list(item.one = "Hello Kitty")
  function_output <- unlist_if_required(object = function_input)
  expect_equal(function_output, "Hello Kitty")
  expect_true(is.character(function_output))
  function_input <- list(item.one = 42)
  function_output <- unlist_if_required(object = function_input)
  expect_equal(function_output, 42)
  expect_true(is.numeric(function_output))
})

test_that("check unlist_if_required with more than one item lists", {
  function_input <- list(
    item.one = "Hello Kitty",
    item.two = "Island Adventure"
  )
  expect_error(
    unlist_if_required(function_input)
  )
  function_input <- list(
    item.one = "Hello Kitty",
    data = dummy_gasprice
  )
  expect_error(
    unlist_if_required(function_input)
  )
  function_input <- list(
    data.one = dummy_gasprice,
    data.two = dummy_gasprice
  )
  expect_error(
    unlist_if_required(function_input)
  )
  function_input <- list(
    item.one = 1,
    item.two = 2,
    item.three = 3
  )
  expect_error(
    unlist_if_required(function_input)
  )
  function_input <- list(
    item.one = 1,
    item.two = "Hello Kitty",
    data = dummy_gasprice
  )
  expect_error(
    unlist_if_required(function_input)
  )
})

test_that("check unlist_if_required when empty list is given", {
  function_input <- list()
  function_output <- unlist_if_required(function_input)
  expect_equal(function_output,function_input)
  expect_equal(class(function_output), "list")
})

test_that("check unlist_if_required when non-lists are given", {
  function_input <- "Hello Kitty"
  function_output <- unlist_if_required(function_input)
  expect_equal(function_output,function_input)
  expect_true(is.character(function_output))
  function_input <- 42
  function_output <- unlist_if_required(function_input)
  expect_equal(function_output,function_input)
  expect_true(is.numeric(function_output))
  function_input <- dummy_gasprice
  function_output <- unlist_if_required(function_input)
  expect_equal(function_output,function_input)
  expect_true(is.data.frame(function_output))
})
