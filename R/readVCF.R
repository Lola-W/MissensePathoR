#' Create DataFrame for VCF Files
#'
#' This function scans a directory for VCF files matching a given pattern and
#' creates a DataFrame with group, sample, and file path information.
#'
#' @param base_dir Directory containing VCF files.
#' @param vcf_pattern Regular expression to match VCF files.
#' @param group_pattern Regular expression to extract group names from file paths.
#' @param sample_pattern Regular expression to extract sample names from file paths.
#'
#' @return DataFrame with columns: group, sample, file_name.
#' @export
#'
#' @examples
#' vcf_files_path <- system.file("extdata", package = "MissensePathoR")
#' vcf_df <- createVCFDataFrame(vcf_files_path,
#'                              "*.vcf",
#'                              "([0-9]+)h",
#'                              "Rep([0-9]+)")
#'
createVCFDataFrame <- function(base_dir, vcf_pattern, group_pattern, sample_pattern) {
  # Ensure the base directory exists
  if (!dir.exists(base_dir)) {
    stop("Base directory does not exist: ", base_dir)
  }

  # List all files in base directory
  all_files <- list.files(base_dir, pattern = vcf_pattern, full.names = TRUE)

  # Initialize vectors to store group, sample, and file names
  groups <- vector("character", length(all_files))
  samples <- vector("character", length(all_files))
  file_names <- vector("character", length(all_files))

  # Iterate over files and extract group and sample names
  for (i in seq_along(all_files)) {
    file_path <- all_files[i]
    file_names[i] <- file_path

    # Extract group name
    group_match <- regexpr(group_pattern, file_path)
    if (group_match != -1) {
      groups[i] <- regmatches(file_path, group_match)[1]
    } else {
      groups[i] <- NA  # Assign NA if no match found
    }

    # Extract sample name
    sample_match <- regexpr(sample_pattern, file_path)
    if (sample_match != -1) {
      samples[i] <- regmatches(file_path, sample_match)[1]
    } else {
      samples[i] <- NA  # Assign NA if no match found
    }
  }

  # Create a DataFrame
  result_df <- data.frame(group = groups, sample = samples, file_name = file_names, stringsAsFactors = FALSE)

  return(result_df)
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
#' @import data.table
#' @export
#'
#' @examples
#' vcf_files_path <- system.file("extdata", package = "MissensePathoR")
#' vcf_df <- createVCFDataFrame(vcf_files_path,
#'                              "*.vcf",
#'                              "([0-9]+)h",
#'                              "Rep([0-9]+)")
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
      data.table::setDT(data.frame(getFIX(vcf)))
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
    vcf_data[, `:=`(group = unlist(group))] # fixd type
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
