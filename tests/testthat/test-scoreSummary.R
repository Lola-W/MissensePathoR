# Test that 'scoreSummary' returns correct summary statistics
test_that("scoreSummary returns correct summary statistics", {
  testData <- data.table(
    am_pathogenicity = c(0.1, 0.5, 0.3, 0.7, 0.2, 0.4),
    group = c("A", "A", "B", "B", "A", "B"),
    sample_name = c("S1", "S1", "S2", "S2", "S1", "S2")
  )
  category <- "group"
  result <- scoreSummary(testData, category)

  expect_type(result, "list")
})

# Test that 'scoreSummary' handles incorrect inputs
test_that("scoreSummary error upon invalid user input", {
  testData <- data.table(
    am_pathogenicity = c(0.1, 0.5, 0.3, 0.7, 0.2, 0.4),
    group = c("A", "A", "B", "B", "A", "B"),
    sample_name = c("S1", "S1", "S2", "S2", "S1", "S2")
  )

  # Invalid 'data' input
  expect_error(scoreSummary(iris, "Species"))

  # Invalid 'category' input (non-existent column)
  expect_error(scoreSummary(testData, "nonexistent_column"))

  # Invalid 'category' input (not a character)
  expect_error(scoreSummary(testData, list("group")))

  # Missing 'am_pathogenicity' column in data
  testData_no_patho <- copy(testData)[, am_pathogenicity := NULL]
  expect_error(scoreSummary(testData_no_patho, "group"))
})
