#' Attempt to parse a vector into date format
#'
#' \code{parse_date_from_vector} is a function to transform an input vector into
#' a vector of date objects of the format YYYY-MM-DD.
#'
#' @param input A vector of input values, transformable to date objects
#'
#' @return A vector of date objects
#' @export
#'
#' @importFrom lubridate is.Date ymd ydm dmy mdy year
#' @import dplyr
#' @importFrom magrittr '%>%'
#' @importFrom tibble tibble
#'
#' @family Date
#'
#' @examples
#' parse_date_from_vector("10-20-2014")
parse_date_from_vector <- function(input) {
  # Check argument
  if (!(length(input) > 0 && is.null(dim(input)))) {
    stop("Input must be a non-empty vector")
  }
  # If date, then return
  if (lubridate::is.Date(input)) return(input)
  # List all functionsinput to be used for conversion
  conversion_functions <- c(
    "period_to_last_day",
    "lubridate::ymd",
    "lubridate::ydm",
    "lubridate::dmy",
    "lubridate::mdy"
  )
  # Initialize list to collect outcomes
  results_attempts <- tibble::tibble(
    functions = conversion_functions,
    na_values = NA_real_
  )
  # Try them out sequentially on the input
  for (fn in conversion_functions) {
    formula <- paste0(fn,"(input)")
    attempt <- suppressWarnings(
      try(
        expr = eval(parse(text = formula)),
        silent = TRUE
      )
    )
    # If format is correct, check for <1900 year formats and convert to NA's
    if (lubridate::is.Date(attempt)) {
      attempt[which(lubridate::year(attempt) < 1900)] <- NA
    }
    # If format is correct, add up NA values and store
    if (lubridate::is.Date(attempt) && (sum(is.na(attempt)) < length(input))) {
      results_attempts <- results_attempts %>%
        dplyr::mutate(
          na_values = case_when(
            functions == fn ~ sum(is.na(attempt)),
            TRUE ~ as.integer(na_values)
          )
        )
    }
  }
  # If all formats failed, return with error message
  if (all(is.na(unique(results_attempts$na_values)))) {
    stop("The provided input does not correspond to any of the implemented date parsing formats ...")
  }
  # Choose function with lowest na values
  best_fn <- results_attempts %>%
    dplyr::filter(!is.na(na_values)) %>%
    dplyr::filter(na_values == min(na_values)) %>%
    dplyr::pull(functions)
  # If still more than one best_fn, stop
  if (length(best_fn) > 1) {
    message <- paste0("More than one function available for parsing (which is unlikely), so no parsing is done. Suitable functions are:\n ", paste0("- ",best_fn, collapse = "\n"))
    stop(message)
  }
  # Parse and return
  output <- suppressWarnings(
    try(
      expr = eval(parse(text = paste0(best_fn,"(input)"))),
      silent = TRUE
    )
  )
  return(output)
}
