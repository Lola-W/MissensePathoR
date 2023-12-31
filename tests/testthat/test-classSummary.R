# Test that 'classSummary' returns correct count summaries
test_that("classSummary returns correct count summaries", {
  testData <- data.table(
    am_class = c("pathogenic", "benign", "pathogenic", "benign", "benign", "pathogenic"),
    group = c("A", "A", "B", "B", "A", "B")
  )

  result <- classSummary(testData)

  expect_equal(nrow(result), length(unique(testData$group)))

  # Check if summary contains the correct groups and classes
  expect_equal(sort(unique(result$group)), sort(unique(testData$group)))
  expect_true(all(c("pathogenic", "benign") %in% names(result)))

  # Check for correct counts
  expect_equal(as.numeric(result[1,3]), 1L)
  expect_equal(as.numeric(result[1,2]), 2L)
  expect_equal(as.numeric(result[2,3]), 2L)
  expect_equal(as.numeric(result[2,2]), 1L)
})

# Test that 'classSummary' handles incorrect inputs
test_that("classSummary error upon invalid user input", {
  testData <- data.table(
    am_class = c("pathogenic", "benign", "pathogenic", "benign", "benign", "pathogenic"),
    group = c("A", "A", "B", "B", "A", "B")
  )

  # Invalid 'data' input (not a data.table)
  expect_error(classSummary(iris))

  # Missing 'am_class' column in data
  testData_no_class <- copy(testData)[, am_class := NULL]
  expect_error(classSummary(testData_no_class))

  # Missing 'group' column in data
  testData_no_group <- copy(testData)[, group := NULL]
  expect_error(classSummary(testData_no_group))
})
