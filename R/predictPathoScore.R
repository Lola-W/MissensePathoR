#' Predict Pathogenicity Scores for VCF Data
#'
#' This function merges variant data from a VCF data.table with Alpha Missense
#' pathogenicity prediction data based on chromosome (CHROM), position (POS),
#' reference allele (REF), and alternate allele (ALT). It reports the number of
#' variants in the input VCF that could not be matched with a prediction and are
#' thus removed in the resulting data.table.
#'
#' @param vcf_data A data table containing variant call format (VCF) data.
#' @param AlphaMissense_data A data table containing Alpha Missense prediction data.
#'
#' @return A merged data.table where each variant is annotated with Alpha Missense
#' pathogenicity scores and classifications. Rows with variants not found in the
#' Alpha Missense data are excluded.
#' @details
#' The merge operation is inner join-like, meaning only rows with matching keys in
#' both `vcf_data` and `AlphaMissense_data` will be included in the output. The keys
#' used for the merge are the genomic coordinates (CHROM, POS) and the alleles (REF, ALT).
#' The function will output a message to the console indicating the number of merged
#' rows and the number of rows from `vcf_data` that were not included in the merge due to
#' missing predictions.
#'
#' @import data.table
#' @export
#' @examples
#' \dontrun{
#'  prediction <- predictPathoScore(vcfSample, AlphaMissenseSample)
#'  head(prediction)
#' }

predictPathoScore <- function(vcf_data, AlphaMissense_data) {
  # Error checking for input types, contributed by ChatGPT-4
  if (!inherits(vcf_data, "data.table")) {
    stop("vcf_data must be a data.table.")
  }
  if (!inherits(AlphaMissense_data, "data.table")) {
    stop("AlphaMissense_data must be a data.table.")
  }

  # Error checking for required columns in vcf_data
  required_cols_vcf <- c("CHROM", "POS", "REF", "ALT")
  if (!all(required_cols_vcf %in% names(vcf_data))) {
    stop("vcf_data is missing required columns: ", paste(required_cols_vcf[!required_cols_vcf %in% names(vcf_data)], collapse = ", "))
  }

  # Error checking for required columns in AlphaMissense_data
  required_cols_alpha <- c("CHROM", "POS", "REF", "ALT", "genome", "uniprot_id", "transcript_id", "protein_variant", "am_pathogenicity", "am_class")
  if (!all(required_cols_alpha %in% names(AlphaMissense_data))) {
    stop("AlphaMissense_data is missing required columns: ", paste(required_cols_alpha[!required_cols_alpha %in% names(AlphaMissense_data)], collapse = ", "))
  }

  # Merge based on CHROM, POS, REF, and ALT columns
  merged_data <- vcf_data[AlphaMissense_data,
                          nomatch = 0,
                          on = .(CHROM, POS, REF, ALT)]

  # Report the number of removed rows due to lack of prediction
  removed_rows <- nrow(vcf_data) - nrow(merged_data)
  cat(paste("Number of rows in merged data:", nrow(merged_data),
                "\nNumber of rows removed due to lack of prediction:", removed_rows))

  return(merged_data)
}
