#' Make all (non-excluded) numeric columns character
#'
#' \code{numeric_cols_to_character} is a function to transform the numeric
#' column(s) into character column(s).
#'
#' The function finds the numeric column(s) in a dataset, and transform all the
#' numerical values into character./
#'
#' @param data A data.table, data.frame or tibble object containing the data to
#'   which to transform the numerical column(s) into character.
#' @param exclude A character string or vector of character strings indicating
#'   the column(s) in the data to not consider while transforming the numerical
#'   column(s)
#' @return A tibble of the initial dataset but with the numerical column(s)
#'   transformed into character column(s).
#'
#' @export
#'
#' @importFrom magrittr '%>%'
#' @importFrom purrr map
#' @importFrom data.table as.data.table
#' @import dplyr
#'
#' @examples
#' numeric_cols_to_character(
#'   data = dummy_gasprice,
#'   exclude = c("gasprice", "spotprice")
#' )
numeric_cols_to_character <- function(data, exclude = c()) {
  # Check data and exlcuded object
  check_data_format(
    data = data,
    func_name = "numeric_cols_to_character"
  )
  if (class(exclude) != "character" && is.null(class(exclude))) {
    message <- paste0("The parameter 'exclude' should be a string of character, instead of '",exclude,"' ... ")
    stop(message)
  }

  # Determine the column types
  num_cols_to_transform <- get_column_features(
    data = data,
    extended = F
  ) %>%
    # Keep only non-date/character columns
    dplyr::filter(!type %in% c("Date", "character")) %>%
    # Exclude specified columns
    dplyr::filter(!name %in% exclude) %>%
    # Get vector of column names
    dplyr::pull(name)
  # Create transformation quosures
  num_to_char_quosures <- setNames(
    object = purrr::map(
      .x = dplyr::syms(num_cols_to_transform),
      .f = ~dplyr::quo(as.character(!! .x))
    ),
    nm = num_cols_to_transform
  )
  # Apply transformations and return
  data %>%
    dplyr::mutate(!!! num_to_char_quosures) %>%
    return()
}
