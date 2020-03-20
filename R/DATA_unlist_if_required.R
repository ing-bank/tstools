#' Extract item from a list of length 1
#'
#' \code{unlist_if_required} is a function to take an item, usually a data
#' frame, out of a list of length one.
#'
#' @param object List with only one item in it.
#' @return The item in the list.
#' @export
#'
#' @examples
#' data_in_a_list <- list(data = dummy_gasprice)
#' unlist_if_required(object = data_in_a_list)
unlist_if_required <- function(object) {
  if (any(class(object) == "list")) {
    n_objects <- length(object)
    if (n_objects == 0) return(object)
    if (n_objects == 1) return(object[[1]])
    if (n_objects > 1) {
      message <- paste0("List has length ", n_objects, ", while this function can only unlist a list of length 1 ... ")
      stop(message)
    }
  }
  return(object)
}
