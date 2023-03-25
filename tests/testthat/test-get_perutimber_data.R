context("Testing get_perutimber_data")

test_that("it returns the correct output", {

  splist <- c("Euterpe precatoria var. precatorio",
              "Welfia alfredi",
              "Hibiscus abelmoschus var. betulifolius")

  expected <- data.frame(names_submitted = c("Euterpe precatoria var. precatorio",
                                            "Welfia alfredi"),
                         accepted_name = c("Euterpe precatoria var. precatoria",
                                           "Welfia alfredii"),
                         accepted_name_author = c("", "A.J. Hend. & Villalba"),
                         accepted_family = c("Arecaceae", "Arecaceae"),
                         common_name = c("Huasaí, Saké, Tsáke", "Camona"),
                         accepted_name_rank = c("variety", 'species'),
                         habit = c("Palma arbórea", "Palma arbórea"),
                         plant_height_m = c("8-18(-25) m.", "10-20(-25) m."),
                         regions = c("AM, LO, MD, PA, SM.", "PA."),
                         elevation_m = c("100-1000(-2000)", "1400-1700"),
                         uses = c("madera aserrada", "con aptitud maderable"))

  result <- get_perutimber_data(splist)

  expect_equal(result, expected)
})
