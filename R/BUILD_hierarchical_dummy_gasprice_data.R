#' Dummy hierarchical data based on dummy_gasprice
#'
#' A dummy dataset (based on dummy_gasprice) is used as example for different
#' functions in the tsforecast and tsclean packages that make use of hierarhical
#' information.
#'
#' @format A tibble with 3438 rows and 7 variables: \describe{
#'   \item{year_month}{<date> always last day of the month}
#'   \item{location}{<chr> USA. Indiana, New York and some other sub locations}
#'   \item{oil_company}{<chr> The parent company CompanyC, with its subsidiaries CompanyA and CompanyB}
#'   \item{currency}{<chr> Either EUR or USD}
#'   \item{gasprice}{<dbl> average retail gasoline price per gallon in dollars}
#'   \item{spotprice}{<dbl> spot price of a barrel of West Texas Intermediate (WTI) oil in dollars}
#'   \item{gemprice}{<dbl> random generated data using the chi-squared distribution, with rchisq(df = 42 and ncp = 3)}
#'   \item{level_location}{<dbl> The hierarchical level of the location variable}
#'   \item{level_oil_company}{<dbl> The hierarchical level of the oil_company variable}
#' }
"dummy_hierarchical_gasprice"
