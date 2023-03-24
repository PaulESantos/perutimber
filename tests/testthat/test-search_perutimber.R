context("search_perutimber tests")

test_that("search_perutimber returns a character vector", {
  splist <- c("Swietenia macrophylla", "Carapa guianensis", "Cedrela odorata")
  expect_is(search_perutimber(splist), "character")
})

#test_that("search_perutimber returns the correct accepted names", {
#  splist <- c("Swietenia macrophylla", "Carapa guianensis", "Cedrela odorata")
#  expect_equal(search_perutimber(splist), c("Swietenia macrophylla", "Carapa guianensis", "Cedrela odorata"))
#})
