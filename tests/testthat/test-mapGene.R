mock_data <- data.frame(
  uniprot_id = c("P12345", "Q67890"),
  other_column = 1:2
)

test_that("mapGene returns correct output structure", {
  result <- mapGene(mock_data)

  # Check if the result is a data frame
  expect_true(is.data.frame(result), info = "Result should be a data frame")

  # Check if 'hgnc_gene' column is added
  expect_true("hgnc_gene" %in% colnames(result), info = "Result should have 'hgnc_gene' column")
})
