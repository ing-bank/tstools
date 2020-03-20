#' Transforms a large numeric value to a string.
#'
#' \code{summarize_numeric_value} is a function to convert a large numeric value
#' to thousands, millions, billions or trillions.
#'
#' @param value The numeric value (or values) that needs to be converted.
#' @return A string of rounded numeric value with the corresponding scale.
#' @export
#'
#' @examples
#' summarize_numeric_value(value = 124401240)
summarize_numeric_value <- function(value){

  # Create test object
  test <- suppressWarnings(
    sum(is.na(as.numeric(gsub("\\,", "", value)))) > 0
  )

  # Make sure the value is a convertable object
  if (test) {
    message <- paste0("Input 'value' should be a convertable object (a numeric value or a string containing numbers only) ... \n")
    stop(message)
  }

  # Div indicates the first index of the interval vector in which our value is located.
  div <- findInterval(
    x = abs(as.numeric(gsub("\\,", "", value))),
    # Interval can be enlarged with successive powers (ex: 1e15, 1e18 etc...)
    vec = c(0, 1e3, 1e6, 1e9, 1e12)
  )

  # Do not forget to add the corresponding letter of the power in the return expression.
  result <- paste(
    round(as.numeric(gsub("\\,", "", value))/10^(3*(div - 1)), 2),
    c("","K","M","B","T")[div]
  )

  # Return result
  return(result)
}
