
context("add_hierarchical_lineage")

test_that("check add_hierarchical_lineage with valid inputs for location", {
  function_input <- dummy_hierarchical_gasprice %>% 
    dplyr::select(location, level_location) %>% 
    dplyr::distinct()
  function_output <- add_hierarchical_lineage(
    data = function_input,
    hierarchical_col = "location"
  )
  expect_equal(nrow(function_output), 9)
  expect_equal(ncol(function_output), 4)
  expect_equal(colnames(function_output), c(
    "location", "level_location", "parent_location", "leaf_location"
  ))
  expect_equal(class(function_output$location), "character")
  expect_equal(class(function_output$level_location), "numeric")
  expect_equal(class(function_output$parent_location), "character")
  expect_equal(class(function_output$leaf_location), "numeric")
  expect_equal(function_output$location, c(
    "USA", "New York", "North New York", "South New York", "Bronx", 
    "Queens", "Indiana", "North Indiana", "South Indiana"
  ))
  expect_equal(sum(function_output$level_location), 25)
  expect_equal(min(function_output$level_location), 1)
  expect_equal(max(function_output$level_location), 4)
  expect_equal(function_output$level_location, c(1, 2, 3, 3, 4, 4, 2, 3, 3))
  expect_equal(function_output$parent_location, c(
    NA, "USA", "New York", "New York", "South New York", "South New York", 
    "USA", "Indiana", "Indiana"
  ))
  expect_equal(sum(function_output$leaf_location), 5)
  expect_equal(min(function_output$leaf_location), 0)
  expect_equal(max(function_output$leaf_location), 1)
  expect_equal(function_output$leaf_location, c(0, 0, 1, 0, 1, 1, 0, 1, 1))
  
  # Reverse order of function input
  function_input <- dummy_hierarchical_gasprice %>% 
    dplyr::select(location, level_location) %>% 
    dplyr::distinct() %>% 
    purrr::map_df(rev)
  rev_function_output <- add_hierarchical_lineage(
    data = function_input,
    hierarchical_col = "location"
  )
  expect_identical(function_output, rev_function_output)
})

test_that("check add_hierarchical_lineage with valid inputs for oil_company", {
  function_input <- dummy_hierarchical_gasprice %>% 
    dplyr::select(oil_company, level_oil_company) %>% 
    dplyr::distinct()
  function_output <- add_hierarchical_lineage(
    data = function_input,
    hierarchical_col = "oil_company"
  )
  expect_equal(nrow(function_output), 3)
  expect_equal(ncol(function_output), 4)
  expect_equal(colnames(function_output), c(
    "oil_company", "level_oil_company", "parent_oil_company", "leaf_oil_company"
  ))
  expect_equal(class(function_output$oil_company), "character")
  expect_equal(class(function_output$level_oil_company), "numeric")
  expect_equal(class(function_output$parent_oil_company), "character")
  expect_equal(class(function_output$leaf_oil_company), "numeric")
  expect_equal(function_output$oil_company, c(
    "CompanyC", "CompanyA", "CompanyB"
  ))
  expect_equal(sum(function_output$level_oil_company), 5)
  expect_equal(min(function_output$level_oil_company), 1)
  expect_equal(max(function_output$level_oil_company), 2)
  expect_equal(function_output$level_oil_company, c(1, 2, 2))
  expect_equal(function_output$parent_oil_company, c(
    NA, "CompanyC", "CompanyC"
  ))
  expect_equal(sum(function_output$leaf_oil_company), 2)
  expect_equal(min(function_output$leaf_oil_company), 0)
  expect_equal(max(function_output$leaf_oil_company), 1)
  expect_equal(function_output$leaf_oil_company, c(0, 1, 1))
  
  # Reverse order of function input
  function_input <- dummy_hierarchical_gasprice %>% 
    dplyr::select(oil_company, level_oil_company) %>% 
    dplyr::distinct() %>% 
    purrr::map_df(rev)
  rev_function_output <- add_hierarchical_lineage(
    data = function_input,
    hierarchical_col = "oil_company"
  )
  expect_identical(function_output, rev_function_output)
})

test_that("check add_hierarchical_lineage with invalid inputs", {
  expect_error(
    add_hierarchical_lineage(
      data = "whats-his-face"
    )
  )
  expect_error(
    add_hierarchical_lineage(
      data = 42
    )
  )
  function_input <- dummy_hierarchical_gasprice %>% 
    dplyr::select(currency) %>% 
    dplyr::distinct()
  expect_error(
    add_hierarchical_lineage(
      data = function_input
    )
  )
  expect_error(
    add_hierarchical_lineage(
      data = function_input,
      hierarchical_col = "currency"
    )
  )
  function_input <- dummy_hierarchical_gasprice %>% 
    dplyr::select(location, level_location) %>% 
    dplyr::distinct()
  expect_error(
    add_hierarchical_lineage(
      data = function_input %>% 
        dplyr::filter(FALSE),
      hierarchical_col = "location"
    )
  )
  expect_error(
    add_hierarchical_lineage(
      data = function_input %>% 
        dplyr::rename(fake = location),
      hierarchical_col = "location"
    )
  )
  expect_error(
    add_hierarchical_lineage(
      data = function_input %>% 
        dplyr::rename(fake = level_location),
      hierarchical_col = "location"
    )
  )
  expect_error(
    add_hierarchical_lineage(
      data = function_input %>% 
        dplyr::mutate(level_location = level_location - 1),
      hierarchical_col = "location"
    )
  )
  expect_error(
    add_hierarchical_lineage(
      data = function_input %>% 
        dplyr::mutate(level_location = as.character(level_location)),
      hierarchical_col = "location"
    )
  )
})
