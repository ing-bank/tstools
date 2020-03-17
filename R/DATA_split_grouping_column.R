#' Split grouping column back into separate columns
#'
#' \code{split_grouping_column} is a function to split a single grouping column
#' back into one or more grouping columns.
#'
#' @param data A data.table, data.frame or tibble object that has a column named
#'   "grouping" indicating the grouping information, which has been added using
#'   the tstools::add_grouping_column function.
#' @param sep A character string indicating the separator that was used to
#'   combine the separate elements of the grouping.
#'
#' @return A tibble extended with one or more additional group columns, which
#'   correspond to a split of the 'grouping' column.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @import dplyr
#' @import tibble
#'
#' @examples
#' data_with_grouping_column <- tstools::add_grouping_column(
#'   data = dummy_gasprice,
#'   group_cols = c("state", "oil_company")
#' )
#' split_grouping_column(data = data_with_grouping_column)
split_grouping_column <- function(data, sep = "   &   ") {
  # Check data
  check_data_format(
    data = data,
    func_name = "split_grouping_column",
    req_cols = "grouping"
  )
  # Check separator
  if (!is.character(sep) || length(sep) > 1 || nchar(sep) == 0) {
    message <- paste0("The specified 'sep' to be used a separator should be single non-empty string, instead of: '", paste0(sep, collapse = "/"),"'")
    stop(message)
  }
  # Get all groupings
  groupings <- data %>%
    dplyr::select(grouping) %>%
    dplyr::distinct() %>%
    dplyr::pull()
  # Create emtpy result
  all_groups <- tibble::tibble()
  # Loop over every grouping
  for (grouping in groupings) {
    groups <- unlist(strsplit(grouping, sep))
    group_row <- tibble::tibble(grouping = grouping)
    for (group in groups) {
      if (group == "UNAVAILABLE") {
        group_row[["Group"]] <- "All data"
      } else {
        if (!grepl(" = ", group)) {
          message <- paste0("The group '",group,"' in grouping '",grouping,"' does not have the expected 'GROUP = MEMBER' format")
          stop(message)
        }
        column <- gsub(" = .*", "", group)
        value <- gsub(".* = ", "", group)
        group_row[[column]] <- value
      }
    }
    all_groups <- dplyr::bind_rows(
      all_groups,
      group_row
    )
  }
  # Filter columns that are already available
  missing_cols <- colnames(all_groups)[!colnames(all_groups) %in% colnames(data)]
  all_groups <- all_groups %>%
    dplyr::select(c("grouping", missing_cols))
  # Join all groups to the data and return
  dplyr::inner_join(
      x = data,
      y = all_groups,
      by = "grouping"
    ) %>%
    return()
}
