#' Transform a dataset to a time series object
#'
#' \code{transform_data_to_ts_object} is a function to transform a set of
#' forecast data to a time series object.
#'
#' The function transforms data in the form of a data.table, data.frame or
#' tibble into a time series object. The data should have at least a period
#' column, a col_of_interest_column, a grouping column and any other columns to
#' be used as xreg_cols if applicable. These columns can be created using the
#' \code{initialize_ts_forecast_data} function. The expected seasonality can be
#' specified, which are the number of data points that together compose a season
#' (e.g. 12 for yearly seasonality when using monthly data).
#'
#' @param data A data.table, data.frame or tibble object containing the data to
#'   be used for the forecasting, which should contain the columns 'period',
#'   'col_of_interest', 'grouping' and any other columns to be used as xreg_cols
#'   if applicable.
#' @param seasonal_periods A vector of postive integer values indicating the
#'   number of data points that together compose a season (e.g. c(12,3) for
#'   quarterly and yearly seasonality when using monthly data).
#'
#' @return A time series object containing the values for the column_of_interest
#'   over time, as well as any xreg_cols if applicable.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @importFrom forecast msts
#' @import dplyr
#'
#' @examples
#' dummy_hierarchical_ts_data %>%
#'    dplyr::filter(grouping == "location = USA   &   oil_company = CompanyC   &   currency = EUR") %>%
#'    dplyr::select(-level_location, -level_oil_company) %>%
#'  transform_data_to_ts_object()
transform_data_to_ts_object <- function(data, seasonal_periods = c(12,3)) {
  # Check data
  check_data_format(
    data = data,
    func_name = "transform_data_to_ts_object",
    req_cols = c("period", "col_of_interest", "grouping"),
    unique_value_cols = "grouping"
  )
  # Check seasonal_periods
  if (!is.numeric(seasonal_periods)) {
    message <- paste0("The parameter 'seasonal_periods' should be a (a vector of) positive integer value(s), instead of '",paste0(seasonal_periods, collapse = "/"),"' ... ")
    stop(message)
  }

  # Try to parse period if required
  if (is.numeric(data$period)) {
    data <- data %>%
      dplyr::mutate(
        period := period_to_last_day(period)
      )
  }
  # Check if there are any xreg_cols
  xreg_cols <- colnames(data)
  xreg_cols <- xreg_cols[!xreg_cols %in% c("period", "col_of_interest", "grouping", "original_col_of_interest")]
  # Check if the original col_of_interest is available
  ts_cols <- "col_of_interest"
  if ("original_col_of_interest" %in% colnames(data)) {
    ts_cols <- c(ts_cols, "original_col_of_interest")
  }
  # Determine grouping
  grouping <- unique(data$grouping)

  # Determine start of time series
  date_range <- get_date_range(data, "period")
  # Determine number of months within date range
  req_dates <- seq.Date(
      from = period_to_first_day(date_range$start_ym),
      to = period_to_first_day(date_range$end_ym),
      by = "month"
    ) %>%
    date_to_period()
  # Make sure there is only one row for each different element in period
  nrow_period <- data %>%
    dplyr::select(period) %>%
    dplyr::distinct() %>%
    nrow()
  nrow_data <- nrow(data)
  if (nrow_data > nrow_period) {
    message <- paste0("\nThe specified data can only have one row for each distinct value in the period column, but the data has ",nrow_data," rows while there are ",nrow_period," distinct values for period ... \nPLEASE CHECK FOR ROWS WITH DUPLICATE period VALUES FOR GROUPING:\n",paste0("\t", grouping, collapse = "\n"))
    stop(message)
  }

  # Make sure there are no missing periods
  missing_periods <- req_dates[!req_dates %in% date_to_period(data$period)]
  if (length(missing_periods) > 0) {
    message <- paste0("\nThere are missing values in the period column for: ",paste0(missing_periods, collapse = ", "),"\nPLEASE IMPUTE MISSING AND TRY AGAIN FOR GROUPING:\n",paste0("\t", grouping, collapse = "\n"))
    stop(message)
  }

  # Prepare data to transform
  data_to_transform <- data %>%
    dplyr::arrange(period) %>%
    dplyr::select(c(ts_cols, xreg_cols))
  # Make sure all columns are numeric
  numeric_cols <- data_to_transform %>%
    dplyr::select_if(is.numeric) %>%
    colnames()
  non_numeric_cols <- colnames(data_to_transform)[!colnames(data_to_transform) %in% numeric_cols]
  if (length(non_numeric_cols) > 0) {
    message <- paste0("The following columns in data are not numeric, while this is required for creating a time series object:\n", paste0("\t",non_numeric_cols, collapse = "\n"))
    stop(message)
  }

  # Turn data into time series object
  ts_object <- forecast::msts(
    data = data_to_transform,
    start = c(date_range$start_year, date_range$start_month),
    seasonal.periods = seasonal_periods,
    ts.frequency = 12
  )
  # Add attributes
  attr(ts_object, "seasonality") <- seasonal_periods
  attr(ts_object, "grouping") <- grouping
  attr(ts_object, "xreg_cols") <- xreg_cols
  # Return results
  return(ts_object)
}
