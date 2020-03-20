
context("get_ultimate_leaves")

test_that("check get_ultimate_leaves with valid inputs for location", {
  function_input <- dummy_hierarchical_gasprice %>%
    dplyr::select(location, oil_company, level_location, level_oil_company) %>%
    dplyr::distinct() %>%
    add_hierarchical_lineage(hierarchical_col = "location") %>%
    add_hierarchical_lineage(hierarchical_col = "oil_company") %>%
    add_grouping_column(group_cols = c("location", "oil_company"))

  for (grouping in function_input$grouping) {
    function_output <- get_ultimate_leaves(
      data = function_input,
      hierarchical_col = "location",
      grouping = grouping
    )
    if (grepl("location = USA", grouping)) {
      expect_equal(function_output, c("North Indiana", "South Indiana", "Bronx", "Queens", "North New York"))
    }
    if (grepl("location = New York", grouping)) {
      expect_equal(function_output, c("Bronx", "Queens", "North New York"))
    }
    if (grepl("location = North New York", grouping)) {
      expect_equal(function_output, c("North New York"))
    }
    if (grepl("location = South New York", grouping)) {
      expect_equal(function_output, c("Bronx", "Queens"))
    }
    if (grepl("location = Bronx", grouping)) {
      expect_equal(function_output, c("Bronx"))
    }
    if (grepl("location = Queens", grouping)) {
      expect_equal(function_output, c("Queens"))
    }
    if (grepl("location = Indiana", grouping)) {
      expect_equal(function_output, c("North Indiana", "South Indiana"))
    }
    if (grepl("location = North Indiana", grouping)) {
      expect_equal(function_output, c("North Indiana"))
    }
    if (grepl("location = South Indiana", grouping)) {
      expect_equal(function_output, c("South Indiana"))
    }
    function_output <- get_ultimate_leaves(
      data = function_input,
      hierarchical_col = "oil_company",
      grouping = grouping
    )
    if (grepl("oil_company = CompanyC", grouping)) {
      expect_equal(function_output, c("CompanyA", "CompanyB"))
    }
    if (grepl("oil_company = CompanyA", grouping)) {
      expect_equal(function_output, c("CompanyA"))
    }
    if (grepl("oil_company = CompanyB", grouping)) {
      expect_equal(function_output, c("CompanyB"))
    }
  }
})


test_that("check get_ultimate_leaves with invalid inputs", {
  expect_error(
    get_ultimate_leaves(
      data = "whats-his-face"
    )
  )
  expect_error(
    get_ultimate_leaves(
      data = 42
    )
  )
  function_input <- dummy_hierarchical_gasprice %>%
    dplyr::select(location, oil_company, level_location, level_oil_company) %>%
    dplyr::distinct() %>%
    add_hierarchical_lineage(hierarchical_col = "location") %>%
    add_hierarchical_lineage(hierarchical_col = "oil_company") %>%
    add_grouping_column(group_cols = c("location", "oil_company"))
  expect_error(
    get_ultimate_leaves(
      data = function_input
    )
  )
  expect_error(
    get_ultimate_leaves(
      data = function_input,
      hierarchical_col = "grouping"
    )
  )
  expect_error(
    get_ultimate_leaves(
      data = function_input %>% 
        dplyr::filter(FALSE),
      hierarchical_col = "location"
    )
  )
  expect_error(
    get_ultimate_leaves(
      data = function_input %>% 
        dplyr::rename(fake = location),
      hierarchical_col = "location"
    )
  )
  expect_error(
    get_ultimate_leaves(
      data = function_input %>% 
        dplyr::rename(fake = parent_location),
      hierarchical_col = "location"
    )
  )
  expect_error(
    get_ultimate_leaves(
      data = function_input %>% 
        dplyr::rename(fake = leaf_location),
      hierarchical_col = "location"
    )
  )
  expect_error(
    get_ultimate_leaves(
      data = function_input,
      hierarchical_col = "location",
      grouping = 42
    )
  )
  expect_error(
    get_ultimate_leaves(
      data = function_input,
      hierarchical_col = "location"
    )
  )
  expect_error(
    get_ultimate_leaves(
      data = function_input,
      hierarchical_col = "location",
      grouping = "whats-his-face"
    )
  )
})
