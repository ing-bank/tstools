#' Create the hierarchy matrix
#'
#' \code{create_hierarchy_matrix} is a function that creates the hierarchy
#' matrix. This is a matrix that stores the relevant information about the
#' relationship (i.e. connections) between the different groups in your
#' hierarchy.
#'
#' @param data A tibble containing information on the hierarchy structure. There
#'   should be three variables: the hierarchy groups, their levels ("level") and
#'   whether they are a leaf or not ("leaf"). The assumption is that this input
#'   is the output of the \code{identify_hierarchy_leaves} function.
#' @return A matrix of 1's and 0's where the rows contain all of the group names
#'   and the columns contain all of the leaves.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @importFrom purrr map_df map
#' @import dplyr
#'
#' @examples
#' dummy_hierarchical_gasprice %>%
#'    dplyr::select(location, oil_company, level_location, level_oil_company) %>%
#'    dplyr::distinct() %>%
#'    add_hierarchical_lineage(hierarchical_col = "location") %>%
#'    add_hierarchical_lineage(hierarchical_col = "oil_company") %>%
#'    create_hierarchy_matrix()
create_hierarchy_matrix <- function(data) {
  # Check data
  tstools::check_data_format(
    data = data,
    func_name = "create_hierarchy_matrix"
  )
  # Check data has level, leaf and parent columns
  if (!any(grepl("level_", colnames(data)))) {
    stop(paste0("The column 'level_[hierarchical_col]' with the level information seems to be missing ... \n"))
  }
  if (!any(grepl("leaf_", colnames(data)))) {
    stop(paste0("The column 'leaf_[hierarchical_col]' with the leaves information seems to be missing ... \n"))
  }
  if (!any(grepl("parent_", colnames(data)))) {
    stop(paste0("The column 'parent_[hierarchical_col]' with the parent information seems to be missing ... \n"))
  }
  # Check the level columns
  level_cols <- colnames(data)[grepl("level_", colnames(data))]
  for (level_col in level_cols) {
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
      stop("Top level is not given at one of the end points of the data (first/last row), please check the ordering!")
    }
  }
  # Check the leaf columns
  leaf_cols <- colnames(data)[grepl("leaf_", colnames(data))]
  for (leaf_col in leaf_cols) {
    if (!is.numeric(data[[leaf_col]]) || !all(unique(data[[leaf_col]]) %in% c(0, 1))) {
      stop(paste0("The values in the '",leaf_col,"' column must be binary!"))
    }
  }
  # Check the parent column
  parent_cols <- colnames(data)[grepl("parent_", colnames(data))]
  for (parent_col in parent_cols) {
    if (!is.character(data[[parent_col]]) || sum(is.na(data[[parent_col]])) == 0) {
      stop(paste0("The values in the '",parent_col,"' column must be character, with at least one NA value for the pater familias!"))
    }
  }

  # Extract the variable names for the grouping
  hierarchical_cols <- data %>%
    dplyr::select(
      -dplyr::starts_with("level_"),
      -dplyr::starts_with("parent"),
      -dplyr::starts_with("leaf_")
    ) %>%
    colnames()
  # Add grouping column to the data
  data <- data %>%
    tstools::add_grouping_column(
      group_cols = hierarchical_cols
    )

  # Get vector of all groupings
  groupings <- data %>%
    dplyr::pull(grouping)
  # Get leaves
  leaf_filters <- purrr::map(
    .x = leaf_cols,
    .f = ~ quo(!! sym(.x) == 1)
  )
  leaves <- data %>%
    dplyr::filter(!!! leaf_filters) %>%
    dplyr::pull(grouping)

  # Initialize matrix with zeros
  hierarchy_matrix <- matrix(
    data = NA_real_,
    ncol = length(leaves),
    nrow = length(groupings)
  )
  # Row names are all group observations
  rownames(hierarchy_matrix) <- groupings
  # Column names are only the leaves
  colnames(hierarchy_matrix) <- leaves

  # Loop over parents
  for (grouping in groupings) {
    # Initialize parent data
    temp_data <- data
    # Loop over each hierarchical column
    for (hierarchical_col in hierarchical_cols) {
      # Get ultimate leaves
      ultimate_leaves <- get_ultimate_leaves(
        data = data,
        hierarchical_col = hierarchical_col,
        grouping = grouping
      )
      # Filter ultimate leaves
      temp_data <- temp_data %>%
        dplyr::filter(!! dplyr::sym(hierarchical_col) %in% ultimate_leaves)
    }
    # Find out row number to overwrite
    row_number <- which(rownames(hierarchy_matrix) == grouping)
    # Find out column numbers to overwrite
    col_numbers <- which(colnames(hierarchy_matrix) %in% temp_data$grouping)
    # Initialize row with zeros
    hierarchy_matrix[row_number,] <- 0
    # Overwrite required columns
    hierarchy_matrix[row_number, col_numbers] <- 1
  }

  # Return result
  return(hierarchy_matrix)
}
