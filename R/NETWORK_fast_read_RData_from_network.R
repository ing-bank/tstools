#' Fast read RData from network
#'
#' \code{fast_read_RData_from_network} is a function to more quickly read data
#' from network locations.
#'
#' The function copies an RData file to local disk, before reading the file into
#' memory. Especially for larger files, this is (much) faster than directly
#' reading from a network location.
#'
#' @param file_path A character string indicating the valid path to the file
#'   from which the data is to be loaded (location + name of the file).
#' @param verbose A boolean indicating whether the function should print its
#'   progress or not.
#' @return A data.table, data.frame or tibble object containing the data that
#'   was stored in the RData file.
#'
#' @family Date
#' @export
#'
#' @examples
#' # REPLACE network_location_path/file_name WITH A VALID FILE ON A NETWORK LOCATION
#' fast_read_RData_from_network(
#'   file_path = "network_location_path/file_name.RData",
#'   verbose = TRUE
#' )
fast_read_RData_from_network <- function(file_path, verbose = FALSE) {
  # Record start time
  start <- Sys.time()
  # Define temp file path
  temp_file_path <- file.path(tempdir(), "temp.RData")
  # Copy RData file
  if (verbose) cat(paste0("\n",format(Sys.time(), "%H:%M.%S"),": Copying data file from:\n",file_path))
  copy_success <- file.copy(
    from = file_path,
    to = temp_file_path,
    overwrite = T
  )
  # Check for succesful renaming
  if (!copy_success) {
    message <- paste0("Data could NOT be copied to temp location from:\n",dirname(file_path),"\n\nPlease make sure you have access ... \n")
    stop(message)
  }
  # Reading data from temp
  if (verbose) cat(paste0("\n",format(Sys.time(), "%H:%M.%S"),": Reading data from temporary file:\n",temp_file_path,"\n"))
  data <- readRDS(file = temp_file_path)
  # Message and return results
  if (verbose) message(paste0("Loaded data from file ",basename(file_path)," in ",round(difftime(Sys.time(), start, units = "secs"))," seconds"))
  return(data)
}
