test_that("readVCF handles incorrect input", {
  # Incorrect 'vcf_df' format (missing columns)
  wrong_vcf_df <- data.table(
    sample = "test_sample",
    file_name = "nonexistent_file.vcf"
  )

  # Expect an error due to missing 'group' column
  expect_error(readVCF(wrong_vcf_df), "Input DataFrame must contain columns: 'group', 'sample', 'file_name'")
})
