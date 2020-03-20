#' Plot hierarchy
#'
#' \code{plot_hierarchy} is a function to visualize the hierarchical tree for
#' one of the hierarchical columns available within the hierarchy data. The
#' hierarchy is visualizes as a tree with a layer of nodes for each separate
#' level and edges between the inidividual nodes to indicate (hierarchical)
#' relationships between them.
#'
#' @param hierarchy A named list, containing both the hierarchy matrix as well
#'   as the hierarchy data to be used for constructing the hierarchy
#'   visualization.
#' @param hierarchical_col A character string indicating which column
#'   corresponds to the hierarchical grouping to be visualized.
#' @param interact Boolean, which is set to TRUE if interactions with the
#'   hierarchical tree visualization should be enabled, or set to FALSE if they
#'   should not.
#'
#' @return A visgraph object which visualizes the hierarchical tree for the
#'   specified hierarchical_col within the hierarchy data.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @import dplyr
#' @importFrom visNetwork visNetwork visNodes visEdges visOptions
#'   visHierarchicalLayout visInteraction
#'
#' @examples
#' initialize_ts_forecast_data(
#'     data = dummy_hierarchical_gasprice,
#'     date_col = "year_month",
#'     col_of_interest = "gasprice",
#'     group_cols = "currency",
#'     hierarchical_cols = c("location", "oil_company")
#'   ) %>%
#'   tstools::create_hierarchy_list() %>%
#'   plot_hierarchy(
#'     hierarchical_col = "location"
#'   )
plot_hierarchy <- function(hierarchy, hierarchical_col = "", interact = FALSE) {
  # Check arguments
  if (!is.list(hierarchy)) {
    stop("The specified 'hierarchy' is not a list object as required, but is of class '",paste0(class(hierarchy), collapse  = "/"),"' ... \n")
  }
  if (!identical(c("matrix", "data"), names(hierarchy))) {
    stop("The specified 'hierarchy' is not a list object containing both a 'matrix' and a 'data' object, but instead contains objects called:\n\t",paste0(names(hierarchy), collapse = "\n\t"))
  }
  if (!is.matrix(hierarchy$matrix)) {
    stop("The 'matrix' object within the specified 'hierarchy' does not seem to be a matrix, but is of class '",paste0(class(hierarchy$matrix), collapse  = "/"),"' ... \n")
  }
  if (!is.data.frame(hierarchy$data)) {
    stop("The 'data' object within the specified 'hierarchy' does not seem to be a tibble/data.frame, but is of class '",paste0(class(hierarchy$data), collapse  = "/"),"' ... \n")
  }
  if (!is.character(hierarchical_col) || length(hierarchical_col) != 1) {
    stop("The specified 'hierarchical_col' should be a non-empty character string, instead of class '",paste0(class(hierarchical_col), collapse  = "/"),"' ... \n")
  }
  if (!hierarchical_col %in% comment(hierarchy$matrix)) {
    stop("The specified 'hierarchical_col' (which is '",hierarchical_col,"') does not seem to be available within the specified hierarchy data (which contains only '",paste0(comment(hierarchy$matrix), collapse = "', '"), "')")
  }
  # Define column names
  parent_col <- paste0("parent_", hierarchical_col)
  # Get nodes
  nodes <- hierarchy$data %>%
    dplyr::transmute(
      id = !! sym(hierarchical_col),
      label = !! sym(hierarchical_col),
      title = paste0("<p>",hierarchical_col," = ",!! sym(hierarchical_col),"</p>")
    ) %>%
    dplyr::distinct()
  # Get edges
  edges <- hierarchy$data %>%
    dplyr::transmute(
      from = !! sym(hierarchical_col),
      to = !! sym(parent_col)
    ) %>%
    dplyr::filter(!is.na(to)) %>%
    dplyr::distinct()
  # Create network
  visNetwork::visNetwork(
      nodes = nodes,
      edges = edges
    ) %>%
    visNetwork::visNodes(
      color = list(
        background = "#A8A8A8",
        border = "#A8A8A8",
        highlight = list(
          background = "#FF6200",
          border = "#A8A8A8"
        )
      )
    ) %>%
    visNetwork::visEdges(
      arrows = "to",
      color = "#A8A8A8"
    ) %>%
    visNetwork::visOptions(
      highlightNearest = list(
        enabled = TRUE,
        degree = list(
          from = 100,
          to = 0
        ),
        algorithm = "hierarchical"
      )
    ) %>%
    visNetwork::visHierarchicalLayout(
      direction = "DU",
      sortMethod = "directed"
    ) %>%
    visNetwork::visInteraction(
      dragNodes = interact,
      dragView = interact,
      zoomView = interact
    ) %>%
    return()
}
