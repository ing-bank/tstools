#' Convert period to last day of the month
#'
#' \code{period_to_last_day} is a function to transform a period into the date
#' object for the last day of that month.
#'
#' @param period A period value in yyyymm numeric format.
#' @return A date object, which is the last day of a given month.
#' @export
#'
#' @importFrom lubridate days_in_month
#'
#' @examples
#' period_to_last_day(201805)
period_to_last_day <- function(period) {
  # Leverage on period_to_first_day checks
  first_day <- period_to_first_day(period)
  # Convert to last day of the month
  last_day <- first_day + as.numeric(lubridate::days_in_month(first_day)) - 1
  # Return last_day
  return(last_day)
}
