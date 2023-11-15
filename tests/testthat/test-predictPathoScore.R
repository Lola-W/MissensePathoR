# Create mock VCF data
mockVcfData <- data.table(
  CHROM = c('chr1', 'chr1', 'chr2'),
  POS = c(101, 102, 201),
  REF = c('A', 'G', 'T'),
  ALT = c('G', 'A', 'C'),
  sample_name = c('Sample1', 'Sample2', 'Sample3'),
  group = c('Control', 'Treatment', 'Control'),
  QUAL = c(30, 40, 50)
)

# Create mock Alpha Missense data
mockAlphaMissenseData <- data.table(
  CHROM = c('chr1', 'chr1'),
  POS = c(101, 102),
  REF = c('A', 'G'),
  ALT = c('G', 'A'),
  genome = c('hg38', 'hg38'),
  uniprot_id = c('P12345', 'P67890'),
  transcript_id = c('ENST000001', 'ENST000002'),
  protein_variant = c('p.Ala1Val', 'p.Gly2Asp'),
  am_pathogenicity = c(0.9, 0.1),
  am_class = c('pathogenic', 'benign')
)

# Test that the function returns a merged data.table
test_that("predictPathoScore returns a merged data.table", {
  result <- predictPathoScore(mockVcfData, mockAlphaMissenseData)
  expect_true(inherits(result, "data.table"))
  expect_equal(nrow(result), 2)
})

# Test that the function handles missing columns in vcf_data
test_that("predictPathoScore handles missing columns in vcf_data", {
  incompleteVcfData <- mockVcfData[, .(CHROM, POS, REF)]  # ALT column is missing
  expect_error(predictPathoScore(incompleteVcfData, mockAlphaMissenseData))
})

# Test that the function handles missing columns in AlphaMissense_data
test_that("predictPathoScore handles missing columns in AlphaMissenseData", {
  incompleteAlphaData <- mockAlphaMissenseData[, .(CHROM, POS, REF)]  # ALT and other columns are missing
  expect_error(predictPathoScore(mockVcfData, incompleteAlphaData))
})

# Test that the function handles incorrect data types
test_that("predictPathoScore handles incorrect data types", {
  expect_error(predictPathoScore(as.list(mockVcfData), mockAlphaMissenseData))
  expect_error(predictPathoScore(mockVcfData, as.list(mockAlphaMissenseData)))
})

# Test that the function handles non-matching data
test_that("predictPathoScore handles non-matching data", {
  nonMatchingVcfData <- mockVcfData
  nonMatchingVcfData$CHROM <- 'chr3'  # No matching rows in mockAlphaMissenseData
  result <- predictPathoScore(nonMatchingVcfData, mockAlphaMissenseData)
  expect_equal(nrow(result), 0)
})
