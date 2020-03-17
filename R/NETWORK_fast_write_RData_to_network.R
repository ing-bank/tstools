#' Fast write RData to network
#'
#' \code{fast_write_RData_to_network} is a function to more quickly write data
#' to network locations.
#'
#' The function writes an RData file to local disk, before copying the file to
#' the required network location. Especially for larger files, this is (much)
#' faster than directly writing to a network location.
#'
#' @param data_to_save A data.table, data.frame or tibble object containing the
#'   data to be stored to the network location.
#' @param file_path A character string indicating the valid path to be used when
#'   storing the data (location + name of the file).
#' @param verbose A boolean indicating whether the function should print its
#'   progress or not.
#' @export
#'
#' @examples
#' # REPLACE network_location_path WITH A VALID NETWORK LOCATION
#' fast_write_RData_to_network(
#'   data_to_save = dummy_gasprice,
#'   file_path = "network_location_path/dummy_gasprice.RData",
#'   verbose = TRUE
#' )
fast_write_RData_to_network <- function(data_to_save, file_path, verbose = FALSE) {
  # Record start time
  start <- Sys.time()
  # Define temp file path
  temp_file_path <- file.path(tempdir(), "temp.RData")
  # Save as RData
  if (verbose) cat(paste0("\n",format(Sys.time(), "%H:%M.%S"),": Saving data temporarily in:\n",temp_file_path))
  saveRDS(
    object = data_to_save,
    file = temp_file_path
  )
  # Move temp file to final location
  if (verbose) cat(paste0("\n",format(Sys.time(), "%H:%M.%S"),": Moving temporary data to final location:\n",file_path,"\n"))
  rename_success <- file.rename(
    from = temp_file_path,
    to = file_path
  )
  # Check for succesful renaming
  if (!rename_success) {
    message <- paste0("Data could NOT be moved from temp location to:\n",dirname(file_path),"\n\nPlease make sure you have access ... \n")
    stop(message)
  }
  # Message
  if (verbose) message(paste0("Saved data in file ",basename(file_path)," in ",round(difftime(Sys.time(), start, units = "secs"))," seconds"))
}
