testData <- data.table(
  group = c("Group1", "Group1", "Group2", "Group2"),
  am_class = c("likely_benign", "likely_pathogenic", "likely_benign", "likely_pathogenic"),
  count = c(10, 5, 20, 3)  # Example counts
)

test_that("classVis handles invalid inputs", {
  # Check for error when 'group' column is missing
  testData_no_group <- copy(testData)[, group := NULL]
  expect_error(classVis(testData_no_group))

  # Check for error when 'am_class' column is missing
  testData_no_class <- copy(testData)[, am_class := NULL]
  expect_error(classVis(testData_no_class))

  # Check for error when input is not a data.table
  expect_error(classVis(as.data.frame(testData)))

  # Check for error when specified groups are not present
  expect_error(classVis(testData, groups = c("NonexistentGroup")))
})
