#' Add grouping column to a dataset
#'
#' \code{add_grouping_column} is a function to combine one or more grouping
#' columns into a single grouping column.
#'
#' The function adds a combined grouping column to a dataset, based on a set of
#' specified group columns, which shows for every row which value each group
#' column has.
#'
#' @param data A data.table, data.frame or tibble object containing the data to
#'   which to add the grouping column.
#' @param group_cols A character string or vector of character strings
#'   indicating the column(s) in the data to combined into a single grouping.
#' @return A tibble containing an additional grouping column (called 'grouping')
#'   which for every row contains a summary of the values in each of the
#'   specified group columns.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @importFrom purrr map2
#' @importFrom tidyr unite
#' @import dplyr
#'
#' @examples
#' add_grouping_column(
#'   data = dummy_gasprice,
#'   group_cols = c("state", "oil_company")
#' )
add_grouping_column <- function(data, group_cols) {
  # Check data
  check_data_format(
    data = data,
    func_name = "add_grouping_column",
    req_cols = group_cols
  )
  # Remove previous grouping column if present
  if ("grouping" %in% colnames(data)) {
    data <- data %>%
      dplyr::select(-grouping)
  }
  # Define dplyr::quosures to create grouping column
  paste_group_col_name_with_its_value <- setNames(
    object = purrr::map2(
      .x = group_cols,
      .y = dplyr::syms(group_cols),
      .f = ~dplyr::quo(paste0(!! .x, ' = ', !! .y))
    ),
    nm = paste0(group_cols,'_temp')
  )
  # Create grouping column, join it to the rest of the data and return
  data %>%
    dplyr::select(group_cols) %>%
    dplyr::distinct(.keep_all = T) %>%
    # Apply mutate based on dplyr::quosures
    dplyr::mutate(!!! paste_group_col_name_with_its_value) %>%
    # Paste together all group columns into grouping
    tidyr::unite(
      col = grouping,
      paste0(group_cols,'_temp'),
      sep = "   &   "
    ) %>%
    # Clean up and merge with main dataset
    dplyr::left_join(x = data, y = ., by = group_cols) %>%
    return()
}
