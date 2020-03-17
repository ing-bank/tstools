#' Initialize time series forecast data
#'
#' \code{initialize_ts_forecast_data} is a function to prepare any dataset for
#' use within the time series forecasting framework of the tsforecast
#' package. The function renames a selection of indicated columns to a global
#' format that is used within the time series forecasting framework. Currently
#' only datasets with a monthly time frequency are supported.
#'
#' @param data A data.table, data.frame or tibble object containing the data to
#'   be used for the forecasting.
#' @param date_col A character string indicating the date column in the data,
#'   which can also be in period (YYYYMM) format.
#' @param col_of_interest A character string indicating the column of interest
#'   in the data to be forecasted.
#' @param group_cols A character string or vector of character strings
#'   indicating the column(s) in the data by which to group. The specified
#'   group_cols will be used to create a new column called 'grouping', which
#'   contains every available combination of the specified group_cols (see
#'   example). When continuing with the time series forecasting, every grouping
#'   will be considered separately in both model estimation as well as
#'   performance evaluation.
#' @param xreg_cols A character string or vector of character strings indicating
#'   the column(s) in the data to use as external regressors when forecasting.
#'   When xreg_cols have been specified, multivariate time series forecasting
#'   will be performed (instead of univariate time series forecasting if no
#'   xreg_cols are specified).
#' @param hierarchical_cols A character string or vector of character strings
#'   indicating the columns in the data that give the numeric hierarchical
#'   information about the tree structure of the available levels. Note that the
#'   levels must be in numeric form with increments of one between adjacent
#'   levels. Specify this parameter if you want to do hierarchical time series
#'   forecasting. Also note that there must be a column named
#'   "level_<hierarchical_col>" for each hierarchical column indicated.
#'
#' @return A tibble containing several columns of data required for forecasting
#'   within the time series forecasting framework.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @import dplyr
#'
#' @examples
#' initialize_ts_forecast_data(
#'   data = dummy_gasprice,
#'   date_col = "year_month",
#'   col_of_interest = "gasprice",
#'   group_cols = c("state", "oil_company"),
#'   xreg_cols = c("spotprice", "gemprice")
#' )
initialize_ts_forecast_data <- function(data, date_col = "", col_of_interest = "", group_cols = NULL, xreg_cols = NULL, hierarchical_cols = NULL) {
  # Check data
  check_data_format(
    data = data,
    func_name = "initialize_ts_forecast_data",
    req_cols = c(date_col, col_of_interest)
  )
  # Check the date_col
  if (!any(class(data[[date_col]]) %in% c("Date", "POSIXct", "POSIXt", "POSIXlt"))) {
    # Try to parse the date_col
    if (is.numeric(data[[date_col]])) {
      data <- data %>%
        dplyr::mutate(
          !! dplyr::sym(date_col) := period_to_last_day(!! dplyr::sym(date_col))
        )
    } else {
      message <- paste0("The date_col '",date_col,"' is not of Date class, but of '",paste0(class(data[[date_col]]), collapse = "/"),"' ... \n")
      stop(message)
    }
  }
  # Check the col_of_interest
  if (!is.numeric(data[[col_of_interest]])) stop(paste0("The col_of_interest '",col_of_interest,"' is not of numeric class, but of '",paste0(class(data[[col_of_interest]]), collapse = "/"),"' ... \n"))
  # Check the group_cols
  if (!is.null(group_cols)) {
    if (!all(group_cols %in% colnames(data))) {
      missing_group_cols <- group_cols[!group_cols %in% colnames(data)]
      message <- paste0("The following group_cols is/are not available in the data:\n",paste0("\t",missing_group_cols, collapse = "\n"))
      stop(message)
    }
  }
  # Check the xreg_cols
  if (!is.null(xreg_cols)) {
    if (!all(xreg_cols %in% colnames(data))) {
      missing_xreg_cols <- xreg_cols[!xreg_cols %in% colnames(data)]
      message <- paste0("The following xreg_cols is/are not available in the data:\n",paste0("\t",missing_xreg_cols, collapse = "\n"))
      stop(message)
    }
  }
  # Check the hierarchical_cols
  if (!is.null(hierarchical_cols)) {
    if (!all(hierarchical_cols %in% colnames(data))) {
      stop("The specified hierarchical_cols are not all available in the data ... \n")
    }
    level_cols <- paste0("level_", hierarchical_cols)
    if (!all(level_cols %in% colnames(data))) {
      level_cols <- paste0("LEVEL_", hierarchical_cols)
      if (!all(level_cols %in% colnames(data))) {
        stop("The 'level_[hierarchical_col]' columns corresponding to the specified hierarchical_cols are not all available in the data ... \n")
      }
    }
    for (level_col in level_cols) {
      levels <- data %>%
        dplyr::select(level_col) %>%
        dplyr::distinct() %>%
        dplyr::pull()
      max_level <- max(levels)
      if (!all(is.numeric(levels))) {
        message <- paste0("The '",level_col,"' corresponding to one of the specified hierarchical_cols must be in numeric format ... \n")
        stop(message)
      }
      if (!all(sort(unique(levels)) == 1:max_level)) {
        message <- paste0("The '",level_col,"' corresponding to one of the specified hierarchical_cols must consist of incremental integers, starting from 1 (the highest level) and with increments of one between adjacent levels ... \n")
        stop(message)
      }
    }
  } else {
    level_cols <- NULL
  }
  # Hierarchical cols are also always group cols
  if (!all(hierarchical_cols %in% group_cols)) {
    group_cols <- unique(c(hierarchical_cols, group_cols))
  }
  # Add emtpy grouping if not specified
  if (is.null(group_cols)) {
    data <- data %>%
      dplyr::mutate(group = "all data")
    group_cols <- "group"
  }
  # Combine group columns into grouping column
  data <- add_grouping_column(
    data = data,
    group_cols = group_cols
  )
  # Select the columns with new names and return
  data %>%
    dplyr::select(
      period = !! dplyr::sym(date_col),
      col_of_interest = !! dplyr::sym(col_of_interest),
      grouping = grouping,
      !!! xreg_cols,
      !!! level_cols
    ) %>%
    dplyr::rename_all(tolower) %>%
    return()
}
