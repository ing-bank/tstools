
context("plot_hierarchy")

test_that("check plot_hierarchy with valid inputs", {
  function_input <- dummy_hierarchical_gasprice %>%
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = "currency",
      hierarchical_col = c("location", "oil_company")
    ) %>%
    tstools::create_hierarchy_list()

  function_output <- plot_hierarchy(
    hierarchy = function_input,
    hierarchical_col = "location"
  )
  expect_is(function_output, c("visNetwork", "htmlwidget"))
  expect_equal(names(function_output), c(
    "x", "width", "height", "sizingPolicy", "dependencies", "elementId", "preRenderHook", "jsHooks"
  ))
  expect_is(function_output$x, "list")
  expect_null(function_output$width)
  expect_null(function_output$height)
  expect_is(function_output$sizingPolicy, "list")
  expect_null(function_output$dependencies)
  expect_null(function_output$elementId)
  expect_null(function_output$preRenderHook)
  expect_is(function_output$jsHooks, "list")
  expect_equal(names(function_output$x), c(
    "nodes", "edges", "nodesToDataframe", "edgesToDataframe", "options",
    "groups", "width", "height", "idselection", "byselection", "main",
    "submain", "footer", "background", "highlight", "collapse", "tooltipStay",
    "tooltipStyle"
  ))
  expect_equal(function_output$x$nodes$id, c(
    "USA", "New York", "North New York", "South New York", "Bronx",
    "Queens", "Indiana", "North Indiana", "South Indiana"
  ))
  expect_equal(function_output$x$edges$from, c(
    "New York", "North New York", "South New York", "Bronx", "Queens",
    "Indiana", "North Indiana", "South Indiana"
  ))
  expect_equal(function_output$x$edges$to, c(
    "USA", "New York", "New York", "South New York", "South New York",
    "USA", "Indiana", "Indiana"
  ))
  expect_equal(names(function_output$sizingPolicy), c(
    "defaultWidth", "defaultHeight", "padding", "viewer", "browser", "knitr"
  ))
  expect_equal(function_output$jsHooks, list())

  function_output <- plot_hierarchy(
    hierarchy = function_input,
    hierarchical_col = "oil_company"
  )
  expect_is(function_output, c("visNetwork", "htmlwidget"))
  expect_equal(names(function_output), c(
    "x", "width", "height", "sizingPolicy", "dependencies", "elementId", "preRenderHook", "jsHooks"
  ))
  expect_is(function_output$x, "list")
  expect_null(function_output$width)
  expect_null(function_output$height)
  expect_is(function_output$sizingPolicy, "list")
  expect_null(function_output$dependencies)
  expect_null(function_output$elementId)
  expect_null(function_output$preRenderHook)
  expect_is(function_output$jsHooks, "list")
  expect_equal(names(function_output$x), c(
    "nodes", "edges", "nodesToDataframe", "edgesToDataframe", "options",
    "groups", "width", "height", "idselection", "byselection", "main",
    "submain", "footer", "background", "highlight", "collapse", "tooltipStay",
    "tooltipStyle"
  ))
  expect_equal(function_output$x$nodes$id, c(
    "CompanyC", "CompanyA", "CompanyB"
  ))
  expect_equal(function_output$x$edges$from, c(
    "CompanyA", "CompanyB"
  ))
  expect_equal(function_output$x$edges$to, c(
    "CompanyC", "CompanyC"
  ))
  expect_equal(names(function_output$sizingPolicy), c(
    "defaultWidth", "defaultHeight", "padding", "viewer", "browser", "knitr"
  ))
  expect_equal(function_output$jsHooks, list())
})

test_that("check plot_hierarchy with invalid inputs", {
  function_input <- dummy_hierarchical_gasprice %>%
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = "currency",
      hierarchical_col = c("location", "oil_company")
    ) %>%
    tstools::create_hierarchy_list()
  expect_error(
    plot_hierarchy(
      hierarchy = "fake news"
    )
  )
  expect_error(
    plot_hierarchy(
      hierarchy = list()
    )
  )
  expect_error(
    plot_hierarchy(
      hierarchy = dummy_hierarchical_gasprice
    )
  )
  no_matrix <- function_input
  no_matrix$matrix <- NULL
  expect_error(
    plot_hierarchy(
      hierarchy = no_matrix
    )
  )
  no_data <- function_input
  no_data$data <- NULL
  expect_error(
    plot_hierarchy(
      hierarchy = no_data
    )
  )
  wrong_matrix <- function_input
  wrong_matrix$matrix <- "fake_news"
  expect_error(
    plot_hierarchy(
      hierarchy = wrong_matrix
    )
  )
  wrong_data <- function_input
  wrong_data$data <- "fake_news"
  expect_error(
    plot_hierarchy(
      hierarchy = wrong_data
    )
  )
  expect_error(
    plot_hierarchy(
      hierarchy = function_input
    )
  )
  expect_error(
    plot_hierarchy(
      hierarchy = function_input,
      hierarchical_col = c("location", "oil_company")
    )
  )
  expect_error(
    plot_hierarchy(
      hierarchy = function_input,
      hierarchical_col = 42
    )
  )
  expect_error(
    plot_hierarchy(
      hierarchy = function_input,
      hierarchical_col = "fake_news"
    )
  )
})
