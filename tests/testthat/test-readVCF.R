# tests/testthat/test-readVCF.R
context("readVCF functionality")

# Assume 'example_vcf_data' is the example dataset provided with your package
example_vcf_data <- system.file("extdata", "example_vcf_data", package = "MissensePathoR")

test_that("readVCF returns a data.table", {
  vcf_df <- createVCFDataFrame(example_vcf_data, "*.vcf", "([^/]+)/([^/]+)/(.*\\.vcf)")
  expect_true(is.data.table(readVCF(vcf_df)))
})

test_that("readVCF correctly sets group names", {
  vcf_df <- createVCFDataFrame(example_vcf_data, "*.vcf", "([^/]+)/([^/]+)/(.*\\.vcf)")
  result <- readVCF(vcf_df)
  expect_true(all(result$group %in% vcf_df$group))
})

test_that("readVCF handles non-existing files gracefully", {
  non_existing_file <- data.frame(group = "dummy", sample = "dummy", file_name = "non_existing_file.vcf")
  expect_error(readVCF(non_existing_file), "Error reading VCF file: non_existing_file.vcf")
})

# ... more tests ...
