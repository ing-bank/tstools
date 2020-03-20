#'Get details on start and end dates in a column
#'
#'\code{get_date_range} is a function to summarize the range of dates in a given
#'column.
#'
#'@param data A data.table, data.frame or tibble object containing the specified
#'  date_col.
#'@param date_col A character string indicating the date column in the data.
#'@export
#'
#'@return A list object containing: \itemize{ \item{start_ym}{ - starting date
#'  in yyyymm numeric format} \item{start_year}{ - starting year in yyyy numeric
#'  format} \item{start_month}{ - starting month in mm numeric format}
#'  \item{end_ym}{ - ending date in yyyymm numeric format } \item{end_year}{ -
#'  eending year in yyyy numeric format } \item{end_month}{ - eending month in
#'  mm numeric format} }
#'
#'@family Date
#'
#' @examples
#' get_date_range(
#'   data = dummy_gasprice,
#'   date_col = "year_month"
#')
get_date_range <- function(data, date_col = "") {
  # Check data
  check_data_format(
    data = data,
    func_name = "get_date_range",
    req_cols = date_col
  )
  # Check date_col
  if (is.numeric(data[[date_col]]) & all(unique(nchar(data[[date_col]])) == 6)) {
    start_ym <- min(data[[date_col]])
    end_ym <- max(data[[date_col]])
  } else if (any(class(data[[date_col]]) %in% c("Date", "POSIXct", "POSIXt", "POSIXlt"))) {
    start_ym <- date_to_period(min(data[[date_col]]))
    end_ym <- date_to_period(max(data[[date_col]]))
  } else {
    message <- paste0("The specified date_col '",date_col,"' in the data needs to be in either 'YYYYMM' format or in Date format ... \n")
    stop(message)
  }
  # Create output list
  list(
      start_ym = start_ym,
      start_year = as.numeric(substr(start_ym,1,4)),
      start_month = as.numeric(substr(start_ym,5,6)),
      end_ym = end_ym,
      end_year = as.numeric(substr(end_ym,1,4)),
      end_month = as.numeric(substr(end_ym,5,6))
    ) %>%
    return()
}
