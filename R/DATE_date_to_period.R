#' Convert date object to period
#'
#' \code{date_to_period} is a function to transform a date object into a period
#' (numeric - yyyymm format).
#'
#' @param date A date object
#' @return An object in yyyymm numeric format
#' @export
#'
#' @family Date
#'
#' @examples
#' date_to_period(as.Date("2018-05-09"))
date_to_period <- function(date) {
  # If input is not date format then stop and give message
  if (!any(class(date) %in% c("Date", "POSIXct", "POSIXt", "POSIXlt"))) {
    message <- paste0("Input value '",date,"' is not of Date class, but of '",paste0(class(date), collapse = "/"),"' ... \n")
    stop(message)
  }
  # Convert to %Y%m numeric format
  period <- as.numeric(format.Date(date,"%Y%m"))
  # Return period
  return(period)
}
