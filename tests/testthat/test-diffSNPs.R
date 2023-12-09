test_that("diffSNPs handles invalid input", {
  # Test with missing columns
  invalid_data <- data.frame(sample_name = c("Sample1", "Sample2"),
                             CHROM = c("chr1", "chr2"),
                             POS = c(100, 300),
                             REF = c("A", "G"),
                             ALT = c("T", "C"))

  expect_error(diffSNPs(invalid_data, "Group1"),
               "Data must contain 'group', hgnc_gene', 'sample_name', and 'am_class'.",
               info = "Should throw an error for missing columns")
})
