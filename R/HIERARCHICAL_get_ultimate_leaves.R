#' Get ultimate leaves
#'
#' \code{get_ultimate_leaves} is a function that creates a vector of the
#' ultimate leaves corresponding to one of the columns within a specific
#' grouping, based on data containing the hierarchy information.
#'
#' @param data A tibble containing information on the hierarchy structure. There
#'   should be three variables: the hierarchy groups, their direct parent
#'   ("parent_") and whether they are a leaf or not ("leaf_"). The assumption is
#'   that this input is the output of the \code{add_hierarchical_lineage}
#'   function.
#' @param hierchical_col A character string indicating which column corresponds
#'   to the hierarchy group for which the lineage should be added to the data.
#' @param grouping A character string indicating for which grouping the ultimate
#'   leaves should be returned.
#'
#' @return A vector of character strings corresponding to the ultimate leaves
#'   for the specified grouping.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @import dplyr
#'
#' @examples
#' dummy_hierarchical_gasprice %>%
#'    dplyr::select(location, oil_company, level_location, level_oil_company) %>%
#'    dplyr::distinct() %>%
#'    add_hierarchical_lineage(hierarchical_col = "location") %>%
#'    tstools::add_grouping_column(group_cols = c("location", "oil_company")) %>%
#'    get_ultimate_leaves(
#'       hierarchical_col = "location",
#'       grouping = "location = New York   &   oil_company = CompanyC"
#'    )
get_ultimate_leaves <- function(data, hierarchical_col, grouping) {
  # Define parent and leaf columns
  parent_col <- paste0("parent_", hierarchical_col)
  leaf_col <- paste0("leaf_", hierarchical_col)
  # Check data
  tstools::check_data_format(
    data = data,
    func_name = "get_ultimate_leaves",
    req_cols = c("grouping", parent_col, leaf_col)
  )
  # Check hierarchical_col
  if (!is.character(hierarchical_col) || length(hierarchical_col) != 1) {
    stop("The specified hierarchical_col should be a non-empty character string ... \n")
  }
  # Check grouping
  if (!is.character(grouping) || length(grouping) != 1) {
    stop("The specified grouping should be a non-empty character string ... \n")
  }
  if (!grouping %in% data$grouping) {
    stop(paste0("The specified grouping '",grouping,"' seems to be missing from the data ... \n"))
  }
  # Get all edges
  edges <- data %>%
    dplyr::select(hierarchical_col, parent_col, leaf_col) %>%
    dplyr::distinct()
  # Filter parent data
  parent_data <- data %>%
    dplyr::filter(grouping == !! grouping)
  # Return if parent is already an ultimate child
  if (parent_data[[leaf_col]] == 1) {
    return(parent_data[[hierarchical_col]])
  }
  # Initialize parents to check
  parents_to_check <- parent_data[[hierarchical_col]]
  # Initialize ultimate leaves
  ultimate_leaves <- c()
  # Check all parents to get the ultimate leaves
  while (length(parents_to_check) > 0) {
    # Specify parent that is checked
    current_parent <- parents_to_check[1]
    # Filter leaves
    leaves <- edges %>%
      dplyr::filter(!! dplyr::sym(parent_col) == current_parent)
    # Add ultimate leaves
    ultimate_leaves <- leaves %>%
      dplyr::filter(!! dplyr::sym(leaf_col) == 1) %>%
      dplyr::pull(hierarchical_col) %>%
      append(ultimate_leaves)
    # Add parents_to_check
    parents_to_check <- leaves %>%
      dplyr::filter(!! dplyr::sym(leaf_col) == 0) %>%
      dplyr::pull(hierarchical_col) %>%
      append(parents_to_check)
    # Remove checked parent
    parents_to_check <- parents_to_check[parents_to_check != current_parent]
  }
  # Return ultimate leaves
  return(ultimate_leaves)
}
