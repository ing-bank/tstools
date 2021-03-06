% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DATA_get_feature_with_date_columns.R
\name{get_feature_with_date_columns}
\alias{get_feature_with_date_columns}
\title{Get the features of the column(s) of a dataset, including any suspect date
column}
\usage{
get_feature_with_date_columns(data)
}
\arguments{
\item{data}{A data.table, data.frame or tibble object containing the data to
which to get the column features.}
}
\value{
A tibble containing the name(s) of the column(s) in the data, their
  type, and two other additional columns indicating the number of unique
  values and mising values.
}
\description{
\code{get_feature_with_date_columns} is an extension of the
\code{get_column_features} function. The extension is that a more thorough
feature extraction is done to detect potential Date columns
}
\examples{
get_feature_with_date_columns(
  data = dummy_gasprice
)
}
