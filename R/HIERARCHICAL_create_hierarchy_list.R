#' Create hierarchy list
#'
#' \code{create_hierarchy_list} is a function that creates the named list
#' "hierarchy", which includes both the hierarchical matrix as well as the data
#' used to create that matrix.
#'
#' @param data The initialized forecast data with the hierarchical "level_"
#'   column in it, which has been created using the
#'   \code{initialize_ts_forecast_data} function.
#' @return A named list of two objects - the hierarchy matrix ("matrix") and
#'   data with the hierarhical group names and their respective levels (
#'   "data").
#' @export
#'
#' @importFrom magrittr '%>%'
#' @import dplyr
#'
#' @examples
#' dummy_hierarchical_gasprice %>%
#'    initialize_ts_forecast_data(
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = "currency",
#'       hierarchical_cols = c("location", "oil_company")
#'    ) %>%
#'    create_hierarchy_list()
create_hierarchy_list <- function(data) {
  # Check data
  tstools::check_data_format(
    data = data,
    func_name = "create_hierarchy_list",
    req_cols = c("grouping")
  )
  # Check availabiilty of level columns
  if (!any(grepl("level_", colnames(data)))) stop(paste0("The required column 'level_[hierarchical_col]' is not available in the data, are you sure you used the 'initialize_ts_forecast_data()' function to create it?\n"))
  # Select only data related to grouping
  grouping_data <- data %>%
    dplyr::select(dplyr::starts_with("level_"), grouping) %>%
    tstools::split_grouping_column() %>%
    dplyr::select(-grouping) %>%
    dplyr::distinct()
  # Get hierarchical column names
  hierarchical_cols <- colnames(grouping_data)[grepl("level_", colnames(grouping_data))]
  hierarchical_cols <- gsub("level_", "", hierarchical_cols)
  # Select only data related to hierarchy
  hierarchical_data <- grouping_data %>%
    dplyr::select(hierarchical_cols, paste0("level_", hierarchical_cols)) %>%
    dplyr::distinct()
  # Add hierarchy lineage for each column
  for (hierarchical_col in hierarchical_cols) {
    hierarchical_data <- hierarchical_data %>%
      add_hierarchical_lineage(hierarchical_col)
  }
  # Create hierarchy matrix
  hierarchy_matrix <- hierarchical_data %>%
    create_hierarchy_matrix()
  # Put in hierarchical_cols into the hierarchy_matrix as a comment
  comment(hierarchy_matrix) <- hierarchical_cols
  # Add hierarchical grouping to hierarchical_data
  hierarchical_data <- tstools::add_grouping_column(
    data = hierarchical_data,
    group_cols = hierarchical_cols
  )
  # Put matrix and data into a list and return
  list(
      matrix = hierarchy_matrix,
      data = hierarchical_data
    ) %>%
    return()
}
