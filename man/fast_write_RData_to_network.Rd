% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NETWORK_fast_write_RData_to_network.R
\name{fast_write_RData_to_network}
\alias{fast_write_RData_to_network}
\title{Fast write RData to network}
\usage{
fast_write_RData_to_network(data_to_save, file_path, verbose = FALSE)
}
\arguments{
\item{data_to_save}{A data.table, data.frame or tibble object containing the
data to be stored to the network location.}

\item{file_path}{A character string indicating the valid path to be used when
storing the data (location + name of the file).}

\item{verbose}{A boolean indicating whether the function should print its
progress or not.}
}
\description{
\code{fast_write_RData_to_network} is a function to more quickly write data
to network locations.
}
\details{
The function writes an RData file to local disk, before copying the file to
the required network location. Especially for larger files, this is (much)
faster than directly writing to a network location.
}
\examples{
# REPLACE network_location_path WITH A VALID NETWORK LOCATION
fast_write_RData_to_network(
  data_to_save = dummy_gasprice,
  file_path = "network_location_path/dummy_gasprice.RData",
  verbose = TRUE
)
}
