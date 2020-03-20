#' Get vector of plot colors
#'
#' \code{get_plot_colors} is a function to generate a vector with color
#' codes, which can be used for plotting. Currently, only 23 distinct colors are
#' implemented. Based on recommendations in \url{https://sashat.me/2017/01/11/list-of-20-simple-distinct-colors/}
#'
#' @param n_colors A positive integer value indicating the number of color codes
#'   to return.
#' @return A vector of character strings corresponding to ING color codes
#' @export
#'
#' @importFrom RColorBrewer brewer.pal
#'
#' @examples
#' get_plot_colors(3)
get_plot_colors <- function(n_colors) {
  # Check to make sure delta is a whole number
  if (!(is.numeric(n_colors) & n_colors >= 1 & n_colors == suppressWarnings(as.integer(n_colors)))) {
    message <- paste0("The parameter 'n_colors' should be a positive integer value, instead of '",n_colors,"' ... ")
    stop(message)
  }
  if (n_colors > 23) {
    message <- paste0("Currently, there are only 23 plot colors implemented (while ",n_colors," are needed) ...\nPlease reduce the number of required colours by reducing the number of distinct groups!")
    stop(message)
  }
  plot_colors <- c(
    "#F58231", # Orange
    "#A9A9A9", # Grey
    "#011EB4", # Purple
    "#4363D8", # Blue
    "#F032E6", # Magenta
    "#BFEF45", # Lime
    "#3CB44B", # Green
    "#333333", # 80% Black
    "#FFE119", # Yellow
    "#42D4F4", # Cyan
    "#E6BEFF", # Lavender
    "#800000"  # Brown
  )
  if (n_colors > length(plot_colors)) {
    extra_colors <- n_colors - length(plot_colors)
    extra_colors <- RColorBrewer::brewer.pal(n = max(3, extra_colors), name = "Spectral")
    plot_colors <- c(plot_colors, extra_colors)
  }
  return(plot_colors[1:n_colors])
}
