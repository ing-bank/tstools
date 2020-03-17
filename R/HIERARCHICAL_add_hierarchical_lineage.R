#' Add hierchical lineage to the data
#'
#' \code{add_hierarchical_lineage} is a function that identifies both the direct
#' parents of every node, as well as the ultimate leaf groups in a hierarchy.
#'
#' @param data A tibble containing information on the hierarchy structure. There
#'   should be two variables, one giving the name of the group and the other
#'   it's level. The order can be descending (top down) or descending (bottom
#'   up), but the resulting output will be descending (top down) for
#'   readability.
#' @param hierchical_col A character string indicating which column corresponds
#'   to the hierarchy group for which the lineage should be added to the data.
#'
#' @return A tibble extended with the lineage for the specified hierchical
#'   group.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @importFrom tibble tibble
#' @importFrom purrr map_df
#' @importFrom zoo na.locf
#' @import dplyr
#'
#' @examples
#' dummy_hierarchical_gasprice %>%
#'    dplyr::select(location, level_location) %>%
#'    dplyr::distinct() %>%
#'    add_hierarchical_lineage(hierarchical_col = "location")
add_hierarchical_lineage <- function(data, hierarchical_col = "") {
  # Check data
  tstools::check_data_format(
    data = data,
    func_name = "add_hierarchical_lineage",
    req_cols = hierarchical_col
  )
  # Check hierarchiacl_col
  if (!is.character(hierarchical_col) || length(hierarchical_col) != 1) {
    stop("The specified hierarchical_col should be a single non-empty character string ... \n")
  }

  # Determine column names
  level_col <- paste0("level_", hierarchical_col)
  parent_col <- paste0("parent_", hierarchical_col)
  leaf_col <- paste0("leaf_", hierarchical_col)
  # Check level_col
  if (!level_col %in% colnames(data)) {
    stop(paste0("The required column '",level_col,"' seems to be missing ... \n"))
  }
  # Check that the level variable is numeric
  if (!is.numeric(data[[level_col]]) || any(data[[level_col]] <= 0)) {
    stop(paste0("The values in the '",level_col,"' column must be numeric and non-negative!"))
  }
  # If data is ascending, reverse order
  if (data[[level_col]][1] != min(data[[level_col]])) {
    data <- data %>%
      purrr::map_df(rev)
  }
  # Check that minimal numeric level (i.e. highest level) is in first row
  if (data[[level_col]][1] != min(data[[level_col]])) {
    stop(paste0("Top level of '",level_col,"' is not given at one of the end points of the data (first/last row), please check the ordering!"))
  }
  # Determine range of level
  min_level <- min(data[[level_col]])
  max_level <- max(data[[level_col]])
  # Create empty table of parents
  parents <- tibble::tibble()
  # Add parents for all other levels
  for (lvl in min_level:max_level) {
    parents <- data %>%
      dplyr::select(hierarchical_col, level_col) %>%
      dplyr::distinct() %>%
      dplyr::filter(!! sym(level_col) <= lvl) %>%
      dplyr::mutate(
        parent = dplyr::case_when(
          !! sym(level_col) == (lvl - 1) ~ !! sym(hierarchical_col),
          TRUE ~ NA_character_
        ),
        parent = zoo::na.locf(parent, na.rm = F)
      ) %>%
      dplyr::filter(!! sym(level_col) == lvl) %>%
      dplyr::rename(
        !! sym(parent_col) := parent
      ) %>%
      dplyr::bind_rows(parents)
  }
  # Combine all lineage information and return
  data %>%
    dplyr::left_join(
      x = .,
      y = parents,
      by = c(hierarchical_col, level_col)
    ) %>%
    dplyr::mutate(
      !! sym(leaf_col) := as.numeric(!(!! sym(hierarchical_col)) %in% .[[parent_col]])
    ) %>%
    return()
}
