#' Dummy data of gasprices over time.
#'
#' A dummy dataset (based on expsmooth::gasprice) containing the gasprices over
#' time in two different stats, for two different companies. This data is also
#' used in testing the different functions available in the tsforecast and
#' tsclean packages.
#'
#' @format A tibble with 764 rows and 6 variables: \describe{
#'   \item{year_month}{<date> always last day of the month} \item{state}{<chr>
#'   New York or Indiana (both in USA)} \item{oil_company}{<chr> either CompanyA
#'   or CompanyB} \item{gasprice}{<dbl> average retail gasoline price per gallon
#'   in dollars} \item{spotprice}{<dbl> spot price of a barrel of West Texas
#'   Intermediate (WTI) oil in dollars} \item{gemprice}{<dbl> random generated
#'   data using the chi-squared distribution, with rchisq(df = 42 and ncp = 3)}
#'   }
#' @source \url{http://www.eia.doe.gov}
"dummy_gasprice"
