
# TO REFRESH THE DUMMY_GASPRICE_DATA
# 1: Open tstools Project file
# 2: Load package (Ctrl + Shift + L)
# 3: Source this script

# AT WHICH POINT YOU WOULD HAVE TO ADJUST ALL TESTS ... SO NEVER DO THIS!
# BECAUSE WE DID NOT USE set.seed() IN THE CODE BELOW :(

library(tidyverse)

create_dummy_gasprice_data <- function() {
  # Create empty table to store results in
  dummy_gasprice <- tibble::tibble()
  # Create state column as first group
  for (state in c("Indiana", "New York")) {
    # Create oil company as second group
    for (oil_company in c("CompanyB", "CompanyA")) {
      # Create dataset for combined grouping and add to result table
      dummy_gasprice <- expsmooth::gasprice %>%
        ts_object_to_tibble() %>%
        dplyr::rowwise() %>%
        dplyr::summarise(
          year_month = period_to_last_day(period),
          state = state,
          oil_company = oil_company,
          gasprice = GasPrice * runif(1, 0.8, 1.2),
          spotprice = SpotPrice * runif(1, 0.8, 1.2),
          gemprice = rchisq(1, 42, ncp = 3)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::bind_rows(dummy_gasprice)
    }
  }
  # Store the data in the /data folder of the package
  usethis::use_data(dummy_gasprice, overwrite = T)
}

create_dummy_gasprice_data()
