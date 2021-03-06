% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NETWORK_get_browser_to_use_for_shiny.R
\name{get_browser_to_use_for_shiny}
\alias{get_browser_to_use_for_shiny}
\title{Determine which browser to use for Shiny}
\usage{
get_browser_to_use_for_shiny(
  browsers = c("google-chrome", "chrome", "firefox")
)
}
\arguments{
\item{browsers}{Character vector indicating which browsers should be
considered, where chrome is preferred over firefox if both are available.}
}
\value{
A character string indicating the path to the browser to be used for
  Shiny.
}
\description{
\code{get_browser_to_use_for_shiny} is a function to determine which browsers
are available to host Shiny and return the one that is most suitable (in that
Chrome > Firefox > IE in terms of support of Shiny features).
}
\examples{
get_browser_to_use_for_shiny(verbose = T)
}
