
# TO REFRESH THE DUMMY_HIERARCHICAL_GASPRICE_DATA
# 1: Open tstools Project file
# 2: Load package (Ctrl + Shift + L)
# 3: Source the create_dummy_data.R script
# 4: Source this script

library(tidyverse)

create_dummy_hierarchical_gasprice_data <- function() {
  # For reproducability
  set.seed(42)
  # Change the variable "state" to "location"
  dummy_hierarchical_gasprice <- dummy_gasprice %>%
    dplyr::rename(location = state) %>%
    dplyr::mutate(level_location = 2)
  # Create data file for location = South/North Indiana
  data_sub_indiana <- dummy_hierarchical_gasprice %>%
    dplyr::filter(location != "USA") %>%
    dplyr::mutate(
      level_location = 3,
      location = case_when(
        location == "New York" ~ "North Indiana",
        location == "Indiana" ~ "South Indiana",
        TRUE ~ location
      ),
      gasprice = round(gasprice*round(runif(1, min = 0.9, max = 1.1), digits = 5), digits = 3),
      spotprice = round(spotprice*round(runif(1, min = 0.9, max = 1.1), digits = 5), digits = 3),
      gemprice = round(gemprice*round(runif(1, min = 0.9, max = 1.1), digits = 5), digits = 3)
    )
  # Create data file for location = South/North New York
  data_sub_newyork <- dummy_hierarchical_gasprice %>%
    dplyr::filter(location != "USA") %>%
    dplyr::mutate(
      level_location = 3,
      location = case_when(
        location == "New York" ~ "North New York",
        location == "Indiana" ~ "South New York",
        TRUE ~ location
      ),
      gasprice = round(gasprice*round(runif(1, min = 0.9, max = 1.1), digits = 5), digits = 3),
      spotprice = round(spotprice*round(runif(1, min = 0.9, max = 1.1), digits = 5), digits = 3),
      gemprice = round(gemprice*round(runif(1, min = 0.9, max = 1.1), digits = 5), digits = 3)
    )
  # Create data file for location = Bronx/Queens
  data_sub_south_newyork <- dummy_hierarchical_gasprice %>%
    dplyr::filter(location != "USA") %>%
    dplyr::mutate(
      level_location = 4,
      location = case_when(
        location == "New York" ~ "Bronx",
        location == "Indiana" ~ "Queens",
        TRUE ~ location
      ),
      gasprice = round(gasprice*round(runif(1, min = 0.9, max = 1.1), digits = 5), digits = 3),
      spotprice = round(spotprice*round(runif(1, min = 0.9, max = 1.1), digits = 5), digits = 3),
      gemprice = round(gemprice*round(runif(1, min = 0.9, max = 1.1), digits = 5), digits = 3)
    )
  # Make sure all the groups add up
  parent_south_new_york <- tibble::tibble()
  parent_new_york <- tibble::tibble()
  parent_indiana <- tibble::tibble()
  parent_usa <- tibble::tibble()
  leaf_north_new_york <- data_sub_newyork %>%
    dplyr::filter(location == "North New York")
  leaf_bronx <- data_sub_south_newyork %>%
    dplyr::filter(location == "Bronx")
  leaf_queens <- data_sub_south_newyork %>%
    dplyr::filter(location == "Queens")
  leaf_north_indiana <- data_sub_indiana %>%
    dplyr::filter(location == "North Indiana")
  leaf_south_indiana <- data_sub_indiana %>%
    dplyr::filter(location == "South Indiana")
  # Add up leaves to relevant parents
  parent_south_new_york <- parent_south_new_york %>%
    dplyr::bind_rows(leaf_bronx) %>%
    dplyr::mutate(
      gasprice = leaf_bronx$gasprice + leaf_queens$gasprice,
      spotprice = leaf_bronx$spotprice + leaf_queens$spotprice,
      gemprice = leaf_bronx$gemprice + leaf_queens$gemprice,
      location = "South New York",
      level_location = 3
    )
  parent_new_york <- parent_new_york %>%
    dplyr::bind_rows(leaf_north_new_york) %>%
    dplyr::mutate(
      gasprice = leaf_bronx$gasprice + leaf_queens$gasprice + leaf_north_new_york$gasprice,
      spotprice = leaf_bronx$spotprice + leaf_queens$spotprice + leaf_north_new_york$spotprice,
      gemprice = leaf_bronx$gemprice + leaf_queens$gemprice + leaf_north_new_york$gemprice,
      location = "New York",
      level_location = 2
    )
  parent_indiana <- parent_indiana %>%
    dplyr::bind_rows(leaf_north_indiana) %>%
    dplyr::mutate(
      gasprice = leaf_north_indiana$gasprice + leaf_south_indiana$gasprice,
      spotprice = leaf_north_indiana$spotprice + leaf_south_indiana$spotprice,
      gemprice = leaf_north_indiana$gemprice + leaf_south_indiana$gemprice,
      location = "Indiana",
      level_location = 2
    )
  parent_usa <- parent_usa %>%
    dplyr::bind_rows(leaf_north_new_york) %>%
    dplyr::mutate(
      gasprice = leaf_bronx$gasprice + leaf_queens$gasprice + leaf_north_new_york$gasprice + leaf_north_indiana$gasprice + leaf_south_indiana$gasprice,
      spotprice = leaf_bronx$spotprice + leaf_queens$spotprice + leaf_north_new_york$spotprice + leaf_north_indiana$spotprice + leaf_south_indiana$spotprice,
      gemprice = leaf_bronx$gemprice + leaf_queens$gemprice + leaf_north_new_york$gemprice + leaf_north_indiana$gemprice + leaf_south_indiana$gemprice,
      location = "USA",
      level_location = 1
    )
  # Add all data files into dummy_hierarchical_gasprice, in correct hierarhical order
  dummy_hierarchical_gasprice <- dplyr::bind_rows(
    parent_usa,
    parent_new_york,
    leaf_north_new_york,
    parent_south_new_york,
    leaf_bronx,
    leaf_queens,
    parent_indiana,
    leaf_north_indiana,
    leaf_south_indiana
  )

  # Now add overall oil_company level
  dummy_hierarchical_gasprice <- dummy_hierarchical_gasprice %>%
    dplyr::mutate(oil_company = "CompanyC") %>%
    dplyr::group_by(year_month, location, oil_company, level_location) %>%
    dplyr::summarise(
      gasprice = sum(gasprice),
      spotprice = sum(spotprice),
      gemprice = sum(gemprice)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::bind_rows(dummy_hierarchical_gasprice) %>%
    dplyr::arrange(year_month, match(location, unique(dummy_hierarchical_gasprice$location)))

  # Add levels for oil_company column
  dummy_hierarchical_gasprice <- dplyr::left_join(
      x = dummy_hierarchical_gasprice,
      y = tibble::tibble(
        oil_company = c("CompanyC", "CompanyA", "CompanyB"),
        level_oil_company = c(1, 2, 2)
      ),
      by = "oil_company"
    ) %>%
    dplyr::arrange(year_month, match(location, unique(dummy_hierarchical_gasprice$location)))

  # Now add a non-hierarchical group
  dummy_hierarchical_gasprice <- dplyr::bind_rows(
      dummy_hierarchical_gasprice %>%
        dplyr::mutate(currency = "EUR"),
      dummy_hierarchical_gasprice %>%
        dplyr::mutate(
          currency = "USD",
          gasprice = gasprice * 1.1,
          spotprice = spotprice * 1.2,
          gemprice = gemprice * 1.3
        )
    ) %>%
    dplyr::arrange(
      year_month,
      match(location, unique(dummy_hierarchical_gasprice$location)),
      match(oil_company, c("CompanyC", "CompanyA", "CompanyB"))
    ) %>%
    dplyr::select(year_month, location, oil_company, currency, gasprice, spotprice, gemprice, level_location, level_oil_company)

  # Store the data in the /data folder of the package
  usethis::use_data(dummy_hierarchical_gasprice, overwrite = T)
}

create_dummy_hierarchical_gasprice_data()
