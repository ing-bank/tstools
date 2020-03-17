#' Convert period to quarter
#'
#' \code{period_to_quarter} is a function to transform a period into the numeric
#' value for the corresponding quarter (e.g. 1 for January, February and March,
#' 2 for April, May, June, etc.).
#'
#' @param period A period value in yyyymm numeric format.
#' @return A numeric value, which is the quarter corresponding to the period.
#' @export
#'
#' @family Date
#'
#' @importFrom lubridate month
#'
#' @examples
#' period_to_quarter(201805)
period_to_quarter <- function(period) {
  # Leverage on period_to_first_day checks
  first_day <- period_to_first_day(period)
  # Convert to the month integer
  month <- lubridate::month(first_day)
  # Convert to quarter
  quarter <- ceiling(month / 3)
  return(quarter)
}
