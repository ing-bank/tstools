#' Determine number of months between two periods
#'
#' \code{months_between_periods} is a function to count the number of months
#' between two periods.
#'
#' @param start The starting period in numeric yyyymm format.
#' @param end The ending period in numeric yyyymm format.
#' @return A number indicating the number of months between the start and end
#'   periods.
#' @export
#'
#' @family Date
#'
#' @importFrom lubridate year month
#'
#' @examples
#' months_between_periods(start = 200901, end = 201512)
months_between_periods <- function(start, end) {
  # Make start and end period into first day format
  start <- period_to_first_day(start)
  end <- period_to_first_day(end)
  # Make sure end is after start
  invalid_combos <- (end < start)
  if (sum(invalid_combos) > 0) {
    message <- paste0("Input value 'end' (which is ",end[invalid_combos],") should not be earlier in time than input value 'start' (which is ",start[invalid_combos],") ... \n")
    stop(message)
  }
  years <- (lubridate::year(end) - lubridate::year(start))
  months <- (lubridate::month(end) - lubridate::month(start))
  # Get number of months between them and return
  return(12 * years + months)
}
