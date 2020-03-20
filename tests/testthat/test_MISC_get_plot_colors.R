
context("get_plot_colors")

test_that("check get_plot_colors with valid inputs", {
  plot_colors <- c(
    "#F58231", # Orange
    "#A9A9A9", # Grey
    "#011EB4", # Purple
    "#4363D8", # Blue
    "#F032E6", # Magenta
    "#BFEF45", # Lime
    "#3CB44B", # Green
    "#333333", # 80% Black
    "#FFE119", # Yellow
    "#42D4F4", # Cyan
    "#E6BEFF", # Lavender
    "#800000"  # Brown
  )
  for (i in 1:length(23)) {
    function_output <- get_plot_colors(n_colors = i)
    expect_equal(length(function_output), i)
    expect_equal(class(function_output), "character")
    if (i <= length(plot_colors)) {
      expect_true(all(function_output %in% plot_colors))
    }
  }
  function_output <- get_plot_colors(n_colors = 23)
  expect_equal(length(function_output), 23)
  expect_equal(class(function_output), "character")
})

test_that("check get_plot_colors with invalid n_colors values", {
  expect_error(
    get_plot_colors(n_colors = 0.5)
  )
  expect_error(
    get_plot_colors(n_colors = -0.5)
  )
  expect_error(
    get_plot_colors(n_colors = 5.5)
  )
  expect_error(
    get_plot_colors(n_colors = -5.5)
  )
  expect_error(
    get_plot_colors(n_colors = 24)
  )
  expect_error(
    get_plot_colors(n_colors = -1)
  )
  expect_error(
    get_plot_colors(n_colors = 0)
  )
})
