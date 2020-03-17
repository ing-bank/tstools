#' Checks specified data to have a required format.
#'
#' \code{check_data_format} is a function to check whether the specified data
#' has the same format as is expected by the function in which this check is
#' performed.
#'
#' @param data A tibble containing the data to be checked in terms of required
#'   format.
#' @param req_cols A vector of character strings corresponding to the column(s)
#'   for which it is checked that they are available in the data.
#' @param unique_value_cols A vector of character strings corresponding to the
#'   column(s) for which it is checked that they contain only a single unique
#'   value.
#'
#' @return Nothing.
#' @export
#'
#' @examples
#' check_data_format(dummy_gasprice)
check_data_format <- function(data, func_name = "this function", req_cols = c(), unique_value_cols = c()){
  # Check the class of the data object
  if (!is.data.frame(data)) {
    message <- paste0("The specified data in ", func_name, "() should be in data.frame or tibble format, instead of: '", paste0(class(data), collapse = "/"), "' ...")
    stop(message)
  }
  # Check that func_name is a single character string
  if (!is.character(func_name) || length(func_name) != 1) {
    message <- paste0("The specified func_name (when calling the check_data_format() function) should be a single character string, instead of: '", paste0(class(func_name), collapse = "/"), "' ...")
    stop(message)
  }
  # Check that the required columns are present
  req_cols <- unique(c(req_cols, unique_value_cols))
  available_cols <- colnames(data)
  missing_cols <- req_cols[!req_cols %in% available_cols]
  if (length(missing_cols) > 0) {
    message <- paste0("\nThe following required columns are NOT AVAILABLE in the data for ", func_name, "():\n- ", paste0(missing_cols, collapse = "\n- "), "\n\nOnly the following columns are available:\n- ", paste0(available_cols, collapse = "\n- "))
    stop(message)
  }
  # Check that the specified data is not empty
  if (nrow(data) == 0) {
    message <- paste0("The specified data in ", func_name, "() is empty ...")
    stop(message)
  }
  # Check that the specified columns contain only a single unique value
  for (col in unique_value_cols) {
    if (length(unique(data[[col]])) > 1) {
      message <- paste0("\nThe specified data in ", func_name, "() should be filtered and contain only a SINGLE unique value in the '", col, "' column, instead of:\n- ", paste0(unique(data[[col]]), collapse = "\n- "))
      stop(message)
    }
  }
}
