#' Replace all empty character column values with an indicator
#'
#' \code{indicate_empty_character_cols} is a function to replace all missing
#' values in the column(s) of a dataset with the specified indicator.
#'
#' The function replaces all missing values in the column(s) of a dataset with
#' the specified indicator.
#'
#' @param data A data.table, data.frame or tibble object containing the data to
#'   which to replace the missing values with the specified indicator.
#' @param fill_with A character string indicating what should the mising values
#'   be replaced with.
#' @return A tibble containing the specified character string instead of the
#'   missing values.
#'
#' @export
#'
#' @importFrom purrr map
#' @importFrom magrittr '%>%'
#' @import dplyr
#'
#' @examples
#' indicate_empty_character_cols(
#'   data = dummy_gasprice,
#'   fill_with = "_is_empty"
#' )
indicate_empty_character_cols <- function(data, fill_with = "_is_empty") {
  # Check data
  check_data_format(
    data = data,
    func_name = "indicate_empty_character_cols"
  )
  # If input is not numeric format then stop and give message
  if (!is.character(fill_with)) {
    message <- paste0("Input value '",fill_with,"' is not of character class, but of '",paste0(class(fill_with), collapse = "/"),"' ... \n")
    stop(message)
  }

  # Determine the column types
  char_cols <- get_column_features(
    data = data,
    extended = F
  ) %>%
    # Keep only the character columns
    dplyr::filter(type == "character") %>%
    # Get vector of column names
    dplyr::pull(name)
  # Create transformation quosures
  char_fix_quosures <- setNames(
    object = purrr::map(
      .x = dplyr::syms(char_cols),
      .f = ~dplyr::quo(
        dplyr::case_when(
          !! dplyr::sym(.x) == "" ~ fill_with,
          is.na(!! .x) ~ fill_with,
          TRUE ~ (!! .x)
        )
      )
    ),
    nm = char_cols
  )
  # Apply transformations and return
  data %>%
    dplyr::mutate(!!! char_fix_quosures) %>%
    return()
}
