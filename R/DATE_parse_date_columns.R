#' Parse the date column of a data frame into YYYY-MM-DD format
#'
#' \code{parse_date_columns} is a function that parses columns that are detected
#' to be of type Date and parses them into YYYY-MM-DD POISXct format. The
#' detection is done via the \code{get_feature_with_date_columns} function.
#'
#' @param data A data.table, data.frame or tibble object containing the data to
#'   which to get the column features.
#' @return The input data, with the identified Date columns now converted into
#'   YYYY-MM-DD POSIXct vectors
#'
#' @export
#'
#' @importFrom magrittr '%>%'
#' @import dplyr
#'
#'
#' @examples
#' parse_date_columns(
#'   data = dummy_gasprice
#' )
parse_date_columns <- function(data) {
  # Check data format
  check_data_format(
    data = data,
    func_name = "parse_date_columns"
  )
  # Identify all date columns
  features <- get_feature_with_date_columns(data)
  # Get columns that are potentially Date
  date_columns <- features %>%
    dplyr::filter(type == "Date") %>%
    dplyr::pull(name)
  # If no columns are potentially Date, return data
  if (length(date_columns) == 0) return(data)
  # Create formula
  formula <- setNames(
    object = map(syms(date_columns), ~ quo(parse_date_from_vector(!!.x))),
    nm = date_columns
  )
  # Change columns and return
  data %>%
    dplyr::mutate(!!! formula) %>%
    return()
}
