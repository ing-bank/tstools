context("create_hierarchy_list")

test_that("check create_hierarchy_list with valid univariate inputs", {
  function_input <- dummy_hierarchical_ts_data
  function_output <- create_hierarchy_list(
    data = function_input
  )
  expect_is(function_output, "list")
  expect_equal(names(function_output), c("matrix", "data"))
  expect_is(function_output$matrix, "matrix")
  expect_is(function_output$data, c("tbl_df", "tbl", "data.frame"))
  expect_equal(comment(function_output$matrix), c("location", "oil_company"))
  expected_groupings <- c(
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
  )
  expect_equal(rownames(function_output$matrix), expected_groupings)
  expect_equal(colnames(function_output$matrix), c(
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
  expect_equal(as.numeric(function_output$matrix[,1]), c(
    1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ))
  expect_equal(as.numeric(function_output$matrix[,2]), c(
    1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ))
  expect_equal(as.numeric(function_output$matrix[,3]), c(
    1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ))
  expect_equal(as.numeric(function_output$matrix[,4]), c(
    1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ))
  expect_equal(as.numeric(function_output$matrix[,5]), c(
    1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ))
  expect_equal(as.numeric(function_output$matrix[,6]), c(
    1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ))
  expect_equal(as.numeric(function_output$matrix[,7]), c(
    1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0
  ))
  expect_equal(as.numeric(function_output$matrix[,8]), c(
    1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0
  ))
  expect_equal(as.numeric(function_output$matrix[,9]), c(
    1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0
  ))
  expect_equal(as.numeric(function_output$matrix[,10]), c(
    1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1
  ))
  expect_equal(nrow(function_output$data), 27)
  expect_equal(colnames(function_output$data), c(
    "location", "oil_company",
    "level_location", "level_oil_company",
    "parent_location", "leaf_location",
    "parent_oil_company", "leaf_oil_company",
    "grouping"
  ))
  expect_equal(unique(function_output$data$location), c(
    "USA", "New York", "North New York",
    "South New York", "Bronx", "Queens",
    "Indiana", "North Indiana", "South Indiana"
  ))
  expect_equal(unique(function_output$data$oil_company), c(
    "CompanyC", "CompanyA", "CompanyB"
  ))
  expect_equal(unique(function_output$data$level_location), c(1, 2, 3, 4))
  expect_equal(function_output$data$level_location, c(
    1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 2, 2, 2, 3, 3, 3, 3, 3, 3
  ))
  expect_equal(unique(function_output$data$level_oil_company), c(1, 2))
  expect_equal(function_output$data$level_oil_company, c(
    1, 2, 2, 1, 2, 2, 1, 2, 2, 1, 2, 2, 1, 2, 2, 1, 2, 2, 1, 2, 2, 1, 2, 2, 1, 2, 2
  ))
  expect_equal(unique(function_output$data$parent_location), c(
    NA, "USA", "New York", "South New York", "Indiana"
  ))
  expect_equal(function_output$data$parent_location, c(
    NA, NA, NA, "USA", "USA", "USA", "New York", "New York", "New York",
    "New York", "New York", "New York", "South New York", "South New York",
    "South New York", "South New York", "South New York", "South New York",
    "USA", "USA", "USA", "Indiana", "Indiana", "Indiana", "Indiana",
    "Indiana", "Indiana"
  ))
  expect_equal(unique(function_output$data$leaf_location), c(0, 1))
  expect_equal(function_output$data$leaf_location, c(
    0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1
  ))
  expect_equal(unique(function_output$data$parent_oil_company), c(
    NA, "CompanyC"
  ))
  expect_equal(function_output$data$parent_oil_company, c(
    NA, "CompanyC", "CompanyC", NA, "CompanyC", "CompanyC",
    NA, "CompanyC", "CompanyC", NA, "CompanyC", "CompanyC", NA,
    "CompanyC", "CompanyC", NA, "CompanyC", "CompanyC", NA, "CompanyC",
    "CompanyC", NA, "CompanyC", "CompanyC", NA, "CompanyC", "CompanyC"
  ))
  expect_equal(unique(function_output$data$leaf_oil_company), c(0, 1))
  expect_equal(function_output$data$leaf_oil_company, c(
    0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1
  ))
  expect_equal(unique(function_output$data$grouping), expected_groupings)
  expect_equal(function_output$data$grouping, expected_groupings)
})

test_that("check create_hierarchy_list with valid multivariate inputs", {
  function_input <- dummy_hierarchical_ts_data_w_xregs %>%
    dplyr::filter(grepl("location = New York", grouping))

  function_output <- create_hierarchy_list(
    data = function_input
  )
  expect_is(function_output, "list")
  expect_equal(names(function_output), c("matrix", "data"))
  expect_is(function_output$matrix, "matrix")
  expect_is(function_output$data, c("tbl_df", "tbl", "data.frame"))
  expect_equal(comment(function_output$matrix), c("location", "oil_company"))
  expected_groupings <- c("location = New York   &   oil_company = CompanyC",
    "location = New York   &   oil_company = CompanyA",
    "location = New York   &   oil_company = CompanyB")
  expect_equal(rownames(function_output$matrix), expected_groupings)
  expect_equal(colnames(function_output$matrix), c(
    "location = New York   &   oil_company = CompanyA", "location = New York   &   oil_company = CompanyB"
  ))
  expect_equal(as.numeric(function_output$matrix[,1]), c(1, 1, 0))
  expect_equal(as.numeric(function_output$matrix[,2]), c(1, 0, 1))
  expect_equal(nrow(function_output$data), 3)
  expect_equal(colnames(function_output$data), c("location", "oil_company", "level_location", "level_oil_company",
                                                 "parent_location", "leaf_location", "parent_oil_company", "leaf_oil_company",
                                                 "grouping"))
  expect_equal(unique(function_output$data$oil_company), c(
    "CompanyC", "CompanyA", "CompanyB"
  ))
  expect_equal(unique(function_output$data$level_oil_company), c(1, 2))
  expect_equal(function_output$data$level_oil_company, c(1, 2, 2))
  expect_equal(unique(function_output$data$parent_oil_company), c(
    NA, "CompanyC"
  ))
  expect_equal(function_output$data$parent_oil_company, c(
    NA, "CompanyC", "CompanyC"
  ))
  expect_equal(unique(function_output$data$leaf_oil_company), c(0, 1))
  expect_equal(function_output$data$leaf_oil_company, c(0, 1, 1))
  expect_equal(unique(function_output$data$grouping), expected_groupings)
  expect_equal(function_output$data$grouping, expected_groupings)
})

test_that("check create_hierarchy_list with invalid inputs", {
  no_levels_multi <- dummy_hierarchical_ts_data_w_xregs
  no_levels_uni <- dummy_hierarchical_ts_data
  bad_level_name_input <- dummy_hierarchical_gasprice %>%
    dplyr::select(location, level_location) %>%
    dplyr::distinct() %>%
    dplyr::rename(hellokitty = level_location)
  bad_increment_input <- dummy_hierarchical_gasprice %>%
    dplyr::select(location, level_location) %>%
    dplyr::mutate(level_location = 2*level_location)
  invalid_input <- list(
    "Bubbles", c(1,2,3,"Hello"),
    bad_level_name_input, bad_increment_input
  )
  for (input in invalid_input) {
    expect_error(
      create_hierarchy_list(data = input)
    )
  }
})

