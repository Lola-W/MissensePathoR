test_that("Function throws an error for invalid enrichOut structure", {
  badEnrichOut <- list(foo = "bar") # Invalid structure
  expect_error(pathwayViz(badEnrichOut, "sig"))
})

test_that("Function handles empty GO:BP data gracefully", {
  # Mock an enrichOut with empty sig
  emptySigEnrichOut <- list("sig")
  emptySigEnrichOut$sig$go_bp <- data.frame() # Empty data frame
  expect_error(pathwayViz(emptySigEnrichOut, "sig"))
})
