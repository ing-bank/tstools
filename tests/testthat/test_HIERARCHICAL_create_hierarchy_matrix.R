
context("create_hierarchy_matrix")

test_that("check create_hierarchy_matrix with valid input for only location", {
  # Standard input
  function_input <- dummy_hierarchical_gasprice %>%
    dplyr::select(location, level_location) %>%
    dplyr::distinct() %>%
    add_hierarchical_lineage(hierarchical_col = "location")
  function_output <- create_hierarchy_matrix(data = function_input)
  expect_is(function_output, "matrix")
  expect_equal(rownames(function_output), c(
    "location = USA", "location = New York", "location = North New York", 
    "location = South New York", "location = Bronx", "location = Queens", 
    "location = Indiana", "location = North Indiana", "location = South Indiana"
  ))
  expect_equal(colnames(function_output), c(
    "location = North New York", "location = Bronx", "location = Queens", 
    "location = North Indiana", "location = South Indiana"
  ))
  expect_equal(as.numeric(function_output[,1]), c(1, 1, 1, 0, 0, 0, 0, 0, 0))
  expect_equal(as.numeric(function_output[,2]), c(1, 1, 0, 1, 1, 0, 0, 0, 0))
  expect_equal(as.numeric(function_output[,3]), c(1, 1, 0, 1, 0, 1, 0, 0, 0))
  expect_equal(as.numeric(function_output[,4]), c(1, 0, 0, 0, 0, 0, 1, 1, 0))
  expect_equal(as.numeric(function_output[,5]), c(1, 0, 0, 0, 0, 0, 1, 0, 1))
  # With no New York
  function_input <- dummy_hierarchical_gasprice %>% 
    dplyr::select(location, level_location) %>% 
    dplyr::filter(!location %in% c("New York", "North New York", "South New York", "Bronx", "Queens")) %>% 
    dplyr::distinct() %>% 
    add_hierarchical_lineage(hierarchical_col = "location")
  function_output <- create_hierarchy_matrix(data = function_input)
  expect_is(function_output, "matrix")
  expect_equal(rownames(function_output), c(
    "location = USA", "location = Indiana", "location = North Indiana", "location = South Indiana"
  ))
  expect_equal(colnames(function_output), c(
    "location = North Indiana", "location = South Indiana"
  ))
  expect_equal(as.numeric(function_output[,1]), c(1, 1, 1, 0))
  expect_equal(as.numeric(function_output[,2]), c(1, 1, 0, 1))
  # With no Indiana
  function_input <- dummy_hierarchical_gasprice %>% 
    dplyr::select(location, level_location) %>% 
    dplyr::filter(!location %in% c("Indiana", "North Indiana", "South Indiana")) %>% 
    dplyr::distinct() %>% 
    add_hierarchical_lineage(hierarchical_col = "location")
  function_output <- create_hierarchy_matrix(data = function_input)
  expect_is(function_output, "matrix")
  expect_equal(rownames(function_output), c(
    "location = USA", "location = New York", "location = North New York", 
    "location = South New York", "location = Bronx", "location = Queens"
  ))
  expect_equal(colnames(function_output), c(
    "location = North New York", "location = Bronx", "location = Queens"
  ))
  expect_equal(as.numeric(function_output[,1]), c(1, 1, 1, 0, 0, 0))
  expect_equal(as.numeric(function_output[,2]), c(1, 1, 0, 1, 1, 0))
  expect_equal(as.numeric(function_output[,3]), c(1, 1, 0, 1, 0, 1))
})

test_that("check create_hierarchy_matrix with valid input for only oil_company", {
  # Standard input
  function_input <- dummy_hierarchical_gasprice %>%
    dplyr::select(oil_company, level_oil_company) %>%
    dplyr::distinct() %>%
    add_hierarchical_lineage(hierarchical_col = "oil_company")
  function_output <- create_hierarchy_matrix(data = function_input)
  expect_is(function_output, "matrix")
  expect_equal(rownames(function_output), c(
    "oil_company = CompanyC", "oil_company = CompanyA", "oil_company = CompanyB"
  ))
  expect_equal(colnames(function_output), c(
    "oil_company = CompanyA", "oil_company = CompanyB"
  ))
  expect_equal(as.numeric(function_output[,1]), c(1, 1, 0))
  expect_equal(as.numeric(function_output[,2]), c(1, 0, 1))
  # With no CompanyB
  function_input <- dummy_hierarchical_gasprice %>% 
    dplyr::select(oil_company, level_oil_company) %>% 
    dplyr::filter(!oil_company %in% c("CompanyB")) %>% 
    dplyr::distinct() %>% 
    add_hierarchical_lineage(hierarchical_col = "oil_company")
  function_output <- create_hierarchy_matrix(data = function_input)
  expect_is(function_output, "matrix")
  expect_equal(rownames(function_output), c(
    "oil_company = CompanyC", "oil_company = CompanyA"
  ))
  expect_equal(colnames(function_output), c(
    "oil_company = CompanyA"
  ))
  expect_equal(as.numeric(function_output[,1]), c(1, 1))
  # With no CompanyA
  function_input <- dummy_hierarchical_gasprice %>% 
    dplyr::select(oil_company, level_oil_company) %>% 
    dplyr::filter(!oil_company %in% c("CompanyA")) %>% 
    dplyr::distinct() %>% 
    add_hierarchical_lineage(hierarchical_col = "oil_company")
  function_output <- create_hierarchy_matrix(data = function_input)
  expect_is(function_output, "matrix")
  expect_equal(rownames(function_output), c(
    "oil_company = CompanyC", "oil_company = CompanyB"
  ))
  expect_equal(colnames(function_output), c(
    "oil_company = CompanyB"
  ))
  expect_equal(as.numeric(function_output[,1]), c(1, 1))
})

test_that("check create_hierarchy_matrix with valid input for both location and oil_company", {
  # Standard input
  function_input <- dummy_hierarchical_gasprice %>%
    dplyr::select(location, level_location, oil_company, level_oil_company) %>%
    dplyr::distinct() %>%
    add_hierarchical_lineage(hierarchical_col = "location") %>% 
    add_hierarchical_lineage(hierarchical_col = "oil_company")
  function_output <- create_hierarchy_matrix(data = function_input)
  expect_is(function_output, "matrix")
  expect_equal(rownames(function_output), c(
    "location = USA   &   oil_company = CompanyC", 
    "location = USA   &   oil_company = CompanyA", 
    "location = USA   &   oil_company = CompanyB", 
    "location = New York   &   oil_company = CompanyC", 
    "location = New York   &   oil_company = CompanyA", 
    "location = New York   &   oil_company = CompanyB", 
    "location = North New York   &   oil_company = CompanyC", 
    "location = North New York   &   oil_company = CompanyA", 
    "location = North New York   &   oil_company = CompanyB", 
    "location = South New York   &   oil_company = CompanyC", 
    "location = South New York   &   oil_company = CompanyA", 
    "location = South New York   &   oil_company = CompanyB", 
    "location = Bronx   &   oil_company = CompanyC", 
    "location = Bronx   &   oil_company = CompanyA", 
    "location = Bronx   &   oil_company = CompanyB", 
    "location = Queens   &   oil_company = CompanyC", 
    "location = Queens   &   oil_company = CompanyA", 
    "location = Queens   &   oil_company = CompanyB", 
    "location = Indiana   &   oil_company = CompanyC", 
    "location = Indiana   &   oil_company = CompanyA", 
    "location = Indiana   &   oil_company = CompanyB", 
    "location = North Indiana   &   oil_company = CompanyC", 
    "location = North Indiana   &   oil_company = CompanyA", 
    "location = North Indiana   &   oil_company = CompanyB", 
    "location = South Indiana   &   oil_company = CompanyC", 
    "location = South Indiana   &   oil_company = CompanyA", 
    "location = South Indiana   &   oil_company = CompanyB"
  ))
  expect_equal(colnames(function_output), c(
    "location = North New York   &   oil_company = CompanyA", 
    "location = North New York   &   oil_company = CompanyB", 
    "location = Bronx   &   oil_company = CompanyA", 
    "location = Bronx   &   oil_company = CompanyB", 
    "location = Queens   &   oil_company = CompanyA", 
    "location = Queens   &   oil_company = CompanyB", 
    "location = North Indiana   &   oil_company = CompanyA", 
    "location = North Indiana   &   oil_company = CompanyB", 
    "location = South Indiana   &   oil_company = CompanyA", 
    "location = South Indiana   &   oil_company = CompanyB"
  ))
  expect_equal(as.numeric(function_output[,1]), c(
    1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ))
  expect_equal(as.numeric(function_output[,2]), c(
    1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ))
  expect_equal(as.numeric(function_output[,3]), c(
    1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ))
  expect_equal(as.numeric(function_output[,4]), c(
    1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ))
  expect_equal(as.numeric(function_output[,5]), c(
    1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ))
  expect_equal(as.numeric(function_output[,6]), c(
    1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ))
  expect_equal(as.numeric(function_output[,7]), c(
    1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0
  ))
  expect_equal(as.numeric(function_output[,8]), c(
    1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0
  ))
  expect_equal(as.numeric(function_output[,9]), c(
    1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0
  ))
  expect_equal(as.numeric(function_output[,10]), c(
    1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1
  ))
})

test_that("check create_hierarchy_matrix with invalid inputs", {
  valid_input <- dummy_hierarchical_gasprice %>% 
    dplyr::select(location, level_location) %>% 
    dplyr::distinct() %>% 
    add_hierarchical_lineage(hierarchical_col = "location")
  bad_leaf_name_input <- valid_input %>% 
    dplyr::rename(leaf_leaf = leaf_location)
  bad_level_name_input <- valid_input %>% 
    dplyr::rename(hellokitty = leaf_location)
  factor_leaf_type <- valid_input %>% 
    dplyr::mutate(leaf_location = as.factor(leaf_location))
  bad_leaf_type <- valid_input %>% 
    dplyr::mutate(leaf_location = 2*leaf_location)
  invalid_input <- list(
    dummy_hierarchical_gasprice, dummy_gasprice, 
    "Bubbles", c(1,2,3,"Hello"), 
    bad_level_name_input, bad_leaf_name_input, 
    bad_leaf_type, factor_leaf_type
  )
  for (input in invalid_input) {
    expect_error(
      create_hierarchy_matrix(data = input)
    ) 
  }
})
