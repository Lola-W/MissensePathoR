# mock dataset
testData <- data.table(
  group = c("0h", "0h", "1h", "1h"),
  am_pathogenicity = c(0.0292, 0.0156, 0.0421, 0.0333)
)

# Create the tests
test_that("scoreVis creates a ggplot", {
  plot <- scoreVis(testData)
  expect_true(is.ggplot(plot))
})

test_that("scoreVis contains correct layers", {
  plot <- scoreVis(testData)

  # Check if the plot contains a geom_boxplot layer
  geom_boxplot_layer <- "GeomBoxplot"
  expect_true(any(sapply(plot$layers, function(x) class(x$geom)[1]) == geom_boxplot_layer))
})

test_that("scoreVis handles multiple groups correctly", {
  plot <- scoreVis(testData, groups = c("0h", "1h"))
  expect_true(is.ggplot(plot))

  # Extract data used in the plot
  plot_data <- ggplot_build(plot)$data[[1]]

  # Check if the plot includes specified groups
  expect_equal(length(sort(unique(plot_data$group))), length(c("0h", "1h")))
})

test_that("scoreVis handles invalid inputs", {
  # Check for error when 'group' column is missing
  testData_no_group <- copy(testData)[, group := NULL]
  expect_error(scoreVis(testData_no_group))

  # Check for error when 'am_pathogenicity' column is missing
  testData_no_patho <- copy(testData)[, am_pathogenicity := NULL]
  expect_error(scoreVis(testData_no_patho))

  # Check for error when input is not a data.table
  expect_error(scoreVis(as.data.frame(testData)))

  # Check for error when specified groups are not present
  expect_error(scoreVis(testData, groups = c("NonexistentGroup")))
})
