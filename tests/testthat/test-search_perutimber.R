context("search_perutimber tests")

test_that("search_perutimber returns a character vector", {
  splist <- c("Swietenia macrophylla", "Carapa guianensis", "Cedrela odorata")
    testthat::expect_type(search_perutimber(splist), "character")
})

test_that("search_perutimber returns the correct accepted names", {
  splist <- c("Swietenia macrophylla", "Carapa guianensis", "Cedrela odorata")
  testthat::expect_equal(search_perutimber(splist),
               c("Present", "Present", "Present"))
})
