#' Get user corporate key
#'
#' \code{get_user_corporate_key} is a function to determine the corporate key of
#' the current user of the machine on which the code is being run, based on
#' system environment variables, user info or a system call. If the corporate
#' key can't be determined, then UNKNOWN is returned.
#'
#' @return A character string corresponding to the corporate key of the user
#' @export
#'
#' @importFrom magrittr '%>%'
#'
#' @examples
#' get_user_corporate_key()
get_user_corporate_key <- function() {
  # Temp helper function
  is_valid_corporate_key <- function(corp_key) grepl("^[[:alpha:]]{2}[[:digit:]]{2}[[:alpha:]]{2}$", corp_key)
  # Try to get corporate key old fashioned way
  user = Sys.getenv("USERNAME")
  # If not valid, try to get corporate key via Sys.info
  if (!is_valid_corporate_key(user)) {
    user = Sys.info()[["user"]]
  }
  # If not valid, try to get corporate key via system call
  if (!is_valid_corporate_key(user)) {
    user <- system("WHOAMI", intern = T) %>%
      gsub("^ad\\\\([[:alpha:]]{2}[[:digit:]]{2}[[:alpha:]]{2})$", "\\1", .)
  }
  # If still empty, set to UNKNOWN
  if (!is_valid_corporate_key(user)) {
    # Filter out the likely folder name
    user <- "UNKNOWN"
  }
  # Return
  user %>%
    toupper() %>%
    return()
}
