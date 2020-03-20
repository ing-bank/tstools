#' Convert period to first day of the month
#'
#' \code{period_to_first_day} is a function to transform a period into the date
#' object for the first day of that month.
#'
#' @param period A period value in yyyymm numeric format.
#' @return A date object, which is the first day of a given month.
#' @export
#'
#' @family Date
#'
#' @examples
#' period_to_first_day(201805)
period_to_first_day <- function(period) {
  # If input is not numeric format then stop and give message
  if (!is.numeric(period)) {
    message <- paste0("Input value '",period,"' is not of numeric class, but of '",paste0(class(period), collapse = "/"),"' ... \n")
    stop(message)
  }
  # If input is not YYYYMM format then stop and give message
  invalid_periods <- (nchar(period) != 6)
  if (sum(invalid_periods) > 0) {
    message <- paste0("Input value '",period[invalid_periods],"' is not of the required 6-digit YYYYMM format ... \n")
    stop(message)
  }
  # If input is not YYYYMM format then stop and give message
  month <- as.numeric(substr(period, 5, 6))
  if (any(month < 1 | month > 12)) {
    message <- paste0("Input value '",period,"' does not have a valid month format in YYYYMM ... \n")
    stop(message)
  }
  # Convert to first day of the month
  first_day <- as.Date.character(period * 100 + 1, format = "%Y%m%d")
  # Return first_day
  return(first_day)
}
