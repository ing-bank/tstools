#' Add/subtract months from a period
#'
#' \code{period_delta} is a function to add or subtract a number of months to or
#' from a given period.
#'
#' @param period The period to start with in numeric yyyymm format.
#' @param delta A positive or negative integer value indicating the number of
#'   months to add or substract.
#' @return A numeric period in yyyymm format resulting from input period +
#'   delta.
#' @export
#'
#' @importFrom magrittr '%>%'
#'
#' @examples
#' period_delta(200901, 2)
#' period_delta(200901, -2)
period_delta <- function(period, delta = 0) {
  # Check to make sure delta is a whole number
  if (!(is.numeric(delta) & delta == suppressWarnings(as.integer(delta)))) {
    message <- paste0("The parameter 'delta' should be an integer value, instead of '",delta,"' ... ")
    stop(message)
  }
  # Combine period and delta to return
  (period_to_first_day(period) + months(delta)) %>%
    date_to_period() %>%
    return()
}
