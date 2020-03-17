#' Get the features of the column(s) in the dataset
#'
#' \code{get_column_features} is a function to show the type, unique values and
#' missing values of the column(s) in a dataset
#'
#' This function creates a tibble object that contains the name of the column(s)
#' in the dataset along with their class. The number of unique values for each
#' column(s) and how many missing values there are for each one of them can also
#' be given.
#'
#' @param data A data.table, data.frame or tibble object containing the data to
#'   which to get the column features.
#' @param extended Boolean. If TRUE (default), then the number of unique and NA
#'   values for each column is also included in the output.
#' @return A tibble containing the name(s) of the column(s) in the data, their
#'   type, and two other additional columns indicating the number of unique
#'   values and mising values.
#'
#' @export
#'
#' @importFrom purrr map
#' @importFrom magrittr '%>%'
#' @importFrom tibble enframe
#' @importFrom data.table as.data.table
#' @import dplyr
#'
#'
#' @examples
#' get_column_features(
#'   data = dummy_gasprice
#' )
get_column_features <- function(data, extended = TRUE) {
  # Check that the input data is a tibble object
  check_data_format(
    data = data,
    func_name = "get_column_features"
  )
  # Initialize tibble with column types
  column_features <- purrr::map(data, class) %>%
    unlist() %>%
    tibble::enframe(
      name = "name",
      value = "type"
    )
  # Extend column features if required
  if (extended) {
    column_features <- column_features %>%
      dplyr::group_by(name) %>%
      dplyr::mutate(
        # Get number of unique and NA values
        unique_values = length(unique(data %>% dplyr::pull(name))),
        na_values = sum(is.na(data %>% dplyr::pull(name)))
      ) %>%
      dplyr::ungroup()
  }
  # Return results
  return(column_features)
}
