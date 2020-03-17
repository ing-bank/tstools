#' Format big numbers
#'
#' \code{format_big_n} is a function to format big numbers in a pretty way, to
#' be used when plotting.
#'
#' @param x A numeric value to be formatted.
#' @param digits A positive or negative integer value indicating the number of
#'   digits to use when formatting the values.
#' @return A character string with big number format.
#' @export
#'
#' @family
#'
#' @examples
#' format_big_n(123456)
#' format_big_n(-123456789)
#' format_big_n(1234567890)
format_big_n <- function(x, digits = 2) {
  # If input is not numeric format then stop and give message
  if (!is.numeric(x)) {
    message <- paste0("Input value '",x,"' is not of numeric class, but of '",paste0(class(x), collapse = "/"),"' ... \n")
    stop(message)
  }
  # Check to make sure digits is a whole number
  if (!(is.numeric(digits) & digits == suppressWarnings(as.integer(digits)))) {
    message <- paste0("The parameter 'digits' should be an integer value, instead of '",digits,"' ... ")
    stop(message)
  }
  # Check if number is negative
  is_negative <- x < 0
  x <- abs(x)
  # Calculate and apply index of divisions
  div <- findInterval(
    x = x,
    vec = c(1, 1e3, 1e6, 1e9, 1e12)
  )
  power <- 3 * (div - 1)
  value <- x / 10^power
  # Finalize format and return
  value %>%
    round(digits) %>%
    paste0(ifelse(is_negative, "-", ""), .) %>%
    paste0(c(""," K"," M"," B"," T")[div]) %>%
    return()
}
