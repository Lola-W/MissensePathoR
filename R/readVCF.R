#' Create DataFrame for VCF Files
#'
#' This function scans a directory for VCF files matching a given pattern and
#' creates a DataFrame with group, sample, and file path information.
#'
#' @param base_dir Directory containing VCF files.
#' @param vcf_pattern Regular expression to match VCF files.
#' @param sample_pattern Regular expression to extract group and sample names from file paths.
#'
#' @return DataFrame with columns: group, sample, file_name.
#' @export
#'
#' @examples
#' vcf_files_path <- system.file("extdata", package = "MissensePathoR")
#' vcf_df <- createVCFDataFrame(vcf_files_path,
#'                              "*.vcf",
#'                              ".*/([0-9]+h)_(Rep[0-9]+)\\.vcf$")
#'
createVCFDataFrame <- function(base_dir, vcf_pattern, sample_pattern) {
  # List all VCF files recursively
  vcf_files <- list.files(base_dir,
                          pattern = vcf_pattern,
                          recursive = TRUE,
                          full.names = TRUE)

  if (!all(file.exists(vcf_files))) {
    stop("One or more VCF files do not exist.")
  }

  # Extract group and sample names based on sample_pattern
  extract_info <- function(file_path) {
    # Modify this regex based on the expected directory structure
    matches <- stringr::str_match(file_path, sample_pattern)
    list(group = matches[, 2], sample = matches[, 3], file_name = file_path)
  }

  # Apply extract_info to each file and create a data frame
  info_list <- lapply(vcf_files, extract_info)
  df <- do.call(rbind, info_list)
  return(data.frame(df, stringsAsFactors = FALSE))
}

#' Read VCF Files into a Data Table
#'
#' This function processes a set of VCF files described in a DataFrame and combines
#' them into a single data.table. Each VCF file is associated with a group and sample name.
#'
#' @param vcf_df DataFrame with columns 'group', 'sample', 'file_name'.
#'
#' @return A data.table combining all VCF data.
#' @import vcfR
#' @export
#'
#' @examples
#' vcf_files_path <- system.file("extdata", package = "MissensePathoR")
#' vcf_df <- createVCFDataFrame(vcf_files_path,
#'                              "*.vcf",
#'                              ".*/([0-9]+h)_(Rep[0-9]+)\\.vcf$")
#' vcf_data <- readVCF(vcf_df)
#'
readVCF <- function(vcf_df) {
  # Validate input DataFrame
  if (!all(c("group", "sample", "file_name") %in% names(vcf_df))) {
    stop("Input DataFrame must contain columns: 'group', 'sample', 'file_name'.
         Please consider using createVCFDataFrame() to generate input with proper format.")
  }

  # Process each VCF file
  combined_data <- lapply(1:nrow(vcf_df), function(i) {
    row <- vcf_df[i, ]
    file_path <- row$file_name
    group <- row$group
    sample <- row$sample

    # Read VCF
    vcf <- tryCatch({
      vcfR::read.vcfR(as.character(file_path), verbose = FALSE)
    }, error = function(e) {
      stop("Error reading VCF file: ", file_path, "\n",
           "Check the integrity of your input file. \n", e$message)
    })

    vcf_data <- tryCatch({
      setDT(data.frame(getFIX(vcf)))
    }, error = function(e) {
      stop("Error processing VCF data for file: ", file_path, "\n", e$message)
    })

    # Check for SNPs and provide a user-friendly warning if non-SNPs are detected
    if (any(nchar(vcf_data$ALT) > 1)) {
      warning("Your VCF file contains variants that are not single nucleotide polymorphisms (SNPs). ",
              "Non-SNPs have been discarded. For SNP analysis, please extract SNPs first using: ",
              "`bcftools view -v snps yourfile.vcf > snp.vcf`.")
      vcf_data <- vcf_data[nchar(vcf_data$ALT) == 1]
    }

    # 2 possible formats of vcf files, adjust CHROM values if needed
    vcf_data[, CHROM := ifelse(grepl("^chr", CHROM), CHROM, paste0("chr", CHROM))]
    vcf_data[, POS := as.integer(POS)]
    vcf_data[, `:=`(group = group)]
    vcf_data[, sample_name := sample]

    return(vcf_data)
  })

  # Bind all data.tables into one
  final_data <- tryCatch({
    rbindlist(combined_data)
  }, error = function(e) {
    stop("Error combining VCF data: ", e$message)
  })

  return(final_data)
}
