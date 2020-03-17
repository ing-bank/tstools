#' Get the features of the column(s) of a dataset, including any suspect date
#' column
#'
#' \code{get_feature_with_date_columns} is an extension of the
#' \code{get_column_features} function. The extension is that a more thorough
#' feature extraction is done to detect potential Date columns
#'
#' @param data A data.table, data.frame or tibble object containing the data to
#'   which to get the column features.
#' @return A tibble containing the name(s) of the column(s) in the data, their
#'   type, and two other additional columns indicating the number of unique
#'   values and mising values.
#'
#' @export
#'
#' @importFrom tidyr drop_na
#' @importFrom magrittr '%>%'
#' @importFrom data.table as.data.table
#' @import dplyr
#'
#'
#' @examples
#' get_feature_with_date_columns(
#'   data = dummy_gasprice
#' )
get_feature_with_date_columns <- function(data) {
  # Check data
  check_data_format(
    data = data,
    func_name = "get_feature_with_date_column"
  )
  # Get column features
  features_data <- data %>%
    get_column_features()
  # Get all names that are not of type Date
  column_names <- features_data %>%
    dplyr::filter(type != "Date") %>%
    dplyr::pull(name)
  # Initialize potential date columns
  date_columns <- c()
  for (name in column_names) {
    # Get random 20 observations
    observations <- data %>%
      dplyr::select(name) %>%
      dplyr::distinct() %>%
      # Replace = T helps when there are less than 20 distinct values
      dplyr::sample_n(20, replace = T) %>%
      tidyr::drop_na() %>%
      dplyr::pull()
    # Try parse_date
    attempt_parse_date <- suppressWarnings(
      try(
        expr = parse_date_from_vector(observations),
        silent = T
      )
    )
    # If no errors, and NA values given no errors, add column to date_columns
    if (!grepl("Error", attempt_parse_date[1]) | lubridate::is.Date(observations)) {
      date_columns <- c(date_columns, name)
    }
  }
  # If date_columns is not empty, check that all rows in the "date" columns can parse
  if (length(date_columns) != 0) {
    for (name in date_columns) {
      # Get all observations
      observations <- data %>%
        dplyr::select(name) %>%
        dplyr::distinct() %>%
        tidyr::drop_na() %>%
        dplyr::pull()
      # Try parse_date
      attempt_parse_date <- suppressWarnings(
        try(
          expr = parse_date_from_vector(observations),
          silent = T
        )
      )
      # If all are NA values, take out of date_columns
      if (any(is.na(attempt_parse_date))) {
        date_columns <- date_columns[date_columns != name]
      }
    }
  }
  # If date_columns is not empty, adjust featureS_data accordingly
  if (length(date_columns) != 0) {
    features_data <- features_data %>%
      dplyr::mutate(
        type = dplyr::case_when(
          name %in% date_columns ~ "Date",
          TRUE ~ type
        )
      )
  }
  # Return
  return(features_data)
}
