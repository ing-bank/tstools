
context("indicate_empty_character_cols")

test_data <- c("data", "science", NA, "hello", "potato", NA) %>%
  matrix(nrow=2, ncol=3, byrow = TRUE) %>%
  tibble::as_tibble(.name_repair = c("unique"))

test_that("check indicate_empty_character_cols output with valid inputs", {
  valid_inputs <- list(dummy_gasprice, test_data)
  valid_fillings <- c("_is_empty", "berke", "99")
  for (i in length(valid_inputs)) {
    for (j in valid_fillings) {
      function_output <- indicate_empty_character_cols(
        data = valid_inputs[[i]],
        fill_with = j
      )

      expect_true(is.data.frame(function_output))
      expect_equal(dim(function_output), dim(valid_inputs[[i]]))
      expect_equal(sum(is.na(function_output)), 0)
      expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))

      if (all(valid_inputs[[i]] == 1, na.rm = T)){
        expect_equal(nrow(function_output), 764)
        expect_equal(ncol(function_output), 6)
        expect_equal(colnames(function_output), c("year_month", "state", "oil_company", "gasprice", "spotprice",
                                                  "gemprice"))
      }
      if (all(valid_inputs[[i]] == 2)){
        expect_equal(nrow(function_output), 2)
        expect_equal(ncol(function_output), 3)
        expect_equal(dim(function_output), dim(test_data))
        expect_equal(colnames(function_output), c("...1", "...2", "...3"))
        expect_equal(count(test_data, "_is_empty")$n, 2)
      }
    }
  }
})


test_that("check indicate_empty_character_cols output with invalid inputs", {
  invalid_inputs <- list("", 4, NA, NULL)
  invalid_fillings <- c(99, NA, NULL)

  for (i in length(invalid_inputs)) {
    for (j in invalid_fillings) {
      expect_error( function_output <- indicate_empty_character_cols(
        data = valid_inputs[[i]],
        fill_with = j
        )
      )
      }
    }
  })

