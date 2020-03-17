#' Determine which browser to use for Shiny
#'
#' \code{get_browser_to_use_for_shiny} is a function to determine which browsers
#' are available to host Shiny and return the one that is most suitable (in that
#' Chrome > Firefox > IE in terms of support of Shiny features).
#'
#' @param browsers Character vector indicating which browsers should be
#'   considered, where chrome is preferred over firefox if both are available.
#' @return A character string indicating the path to the browser to be used for
#'   Shiny.
#' @export
#'
#' @examples
#' get_browser_to_use_for_shiny(verbose = T)
get_browser_to_use_for_shiny <- function(browsers = c("google-chrome", "chrome", "firefox")) {
  # Check arguments
  browsers <- match.arg(browsers, several.ok = T)
  
  if (Sys.info()[["sysname"]] == "Linux") {
    installed_browsers = Sys.which(browsers)
    for (installed_browser in installed_browsers) {
      if (installed_browser != "") {
        return(installed_browser)
      }
    }
  }
  
  if (Sys.info()[["sysname"]] == "Windows") {
    # Try to get registry path to chrome
    if ("chrome" %in% browsers) {
      chrome_registry <- ""
      try(
        expr = {
          chrome_registry <- readRegistry("SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\chrome.exe", hive = "HLM")
          chrome_registry <- unlist(chrome_registry)
          chrome_registry <- chrome_registry[grepl("chrome.exe", chrome_registry)]
          chrome_registry <- gsub("%ALLUSERSPROFILE%", "C:\\\\ProgramData", chrome_registry)
          chrome_registry <- as.character(chrome_registry)
        },
        silent = TRUE
      )
      # Return if exists
      if (file.exists(chrome_registry)) {
        return(chrome_registry)
      }
    }
    # Try to get registry path to firefox
    if ("firefox" %in% browsers) {
      firefox_registry <- ""
      try(
        expr = {
          firefox_registry <- readRegistry("SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\firefox.exe", hive = "HLM")
          firefox_registry <- unlist(firefox_registry)
          firefox_registry <- firefox_registry[grepl("firefox.exe", firefox_registry)]
          firefox_registry <- gsub("%ALLUSERSPROFILE%", "C:\\\\ProgramData", firefox_registry)
          firefox_registry <- as.character(firefox_registry)
        },
        silent = TRUE
      )
      # Return if exists
      if (file.exists(firefox_registry)) {
        return(firefox_registry)
      }
    }
  }
}