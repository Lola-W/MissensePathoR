library(ggplot2)
# library(tidyverse)
# utils::install.packages("here")
library(dplyr)
library(vcfR)
library(data.table)
library(pbapply)

# usethis::use_package("ggplot2")
# use_package("dplyr")
# usethis::use_package("vcfR")
# usethis::use_package("data.table")

getAlphaMissenseData <- function(path, chrInCHROM = TRUE){
  #path = "data/AlphaMissense/AlphaMissense_hg38.tsv"
  AlphaMissense_data <- data.table::fread(path, sep = "\t")
  # AlphaMissense_data$CHROM <- sub("^chr", "", AlphaMissense_data$`#CHROM`)
  AlphaMissense_data <- AlphaMissense_data %>%
    rename(CHROM = `#CHROM`)
  if (!chrInCHROM){
    AlphaMissense_data <- AlphaMissense_data%>%
      mutate(CHROM = str_replace(CHROM, "^chr", ""))}
  return(AlphaMissense_data)
}

getVCF <- function(path){
  vcf <- read.vcfR(path, verbose = FALSE )
  vcf_data <- setDT(data.frame(getFIX(vcf)))
  vcf_data[, POS := as.integer(POS)]
  return(vcf_data)
}


getPrediction <- function(vcf_data, AlphaMissense_data) {
  # Merge based on CHROM, POS, REF, and ALT columns
  merged_data <- vcf_data[AlphaMissense_data,
                          nomatch = 0,
                          .(sample, group, CHROM, POS, REF, ALT, genome, uniprot_id, transcript_id, protein_variant, am_pathogenicity, am_class, QUAL),
                          on = .(CHROM, POS, REF, ALT)]
  # on = .(CHROM, POS)]

  # Report the number of removed rows
  removed_rows <- nrow(vcf_data) - nrow(merged_data)
  message(paste("Number of rows:", nrow(merged_data),
                "\nNumber of removed rows:", removed_rows))

  return(merged_data)
}

readAndCombineVCFs <- function(base_dir) {
  #//TODO progress bar
  # List all VCF files recursively
  vcf_files <- list.files(base_dir, pattern = "*merged_variants.vcf", recursive = TRUE, full.names = TRUE)

  # Initialize a list to hold summary data
  summary_list <- vector("list", length(vcf_files))

  # Use lapply to read and process each VCF file
  combined_data <- lapply(vcf_files, function(file_path) {
    # Extract sample name and group from file path
    paths <- unlist(strsplit(file_path, "/"))
    sample_name <- paths[length(paths) - 1]
    group <- gsub("_outputs$", "", paths[length(paths) - 2])

    # Read VCF
    vcf_data <- getVCF(file_path)

    # Add sample and group columns
    vcf_data[, `:=`(sample = sample_name, group = group)]

    return(vcf_data)
  })

  # Bind all data.tables into one
  final_data <- rbindlist(combined_data)

  return(final_data)
}

# Summary
summarize_pathogenicity <- function(data) {
  summary_data <- data[, .(
    mean_pathogenicity = mean(am_pathogenicity, na.rm = TRUE),
    median_pathogenicity = median(am_pathogenicity, na.rm = TRUE),
    min_pathogenicity = min(am_pathogenicity, na.rm = TRUE),
    max_pathogenicity = max(am_pathogenicity, na.rm = TRUE),
    sd_pathogenicity = sd(am_pathogenicity, na.rm = TRUE),
  ), by = group]

  return(summary_data)
}

summarize_class <- function(data) {
  summary_data <- data[, .(
    count = .N
  ), by = .(group, am_class)]

  # Pivot the data for a more readable format using dcast from data.table
  summary_data <- dcast(summary_data, group ~ am_class, value.var = "count", fill = 0)

  return(summary_data)
}


# Visualization
scoreVis <- function(data) {
  ggplot(data, aes(x = group, y = am_pathogenicity, fill = group)) +
    geom_boxplot(outlier.size = 1.5, width = 0.6) +
    theme_minimal(base_size = 14) +
    labs(title = "Distribution of Pathogenicity Scores Across Groups",
         y = "Pathogenicity Score",
         x = "Group") +
    theme(legend.position = "right",
          legend.background = element_rect(fill = "white", colour = "black"),
          legend.key = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          axis.title = element_text(face = "bold", size = 14),
          axis.text = element_text(size = 12)) +
    scale_fill_manual(values = c("SST" = "#FC4E07", "PVALB" = "#00AFBB"), name = "Group")
}


classVis <- function(data) {
  # Prepare summarized data
  summarized_data <- data[, .(count = .N), by = .(group, am_class)]

  ggplot(summarized_data, aes(x = group, y = count, fill = am_class)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    theme_minimal(base_size = 14) +
    labs(title = "Distribution of Class Categories Across Groups",
         y = "Count",
         x = "Group") +
    scale_fill_brewer(palette = "Set1", name = "Class Category") +
    theme(legend.position = "right",
          legend.justification = c(1, 0),
          legend.background = element_rect(fill = "white", colour = "black"),
          legend.key = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          axis.title = element_text(face = "bold", size = 14),
          axis.text = element_text(size = 12))
}





# #read
# vcf_data <- getVCF("~/data/Freebayes/snps/F1S4_160721_071_G01merged_snps.vcf")
# not_snp_data <- getVCF("~/data/Freebayes/PVALB_outputs/F1S4_160721_071_G01/F1S4_160721_071_G01merged_variants.vcf")
# combined_vcf_data <- combined_vcf_data%>%
#   mutate(CHROM = paste0("chr", CHROM))
# AlphaMissense_data <- getAlphaMissenseData("~/data/AlphaMissense/AlphaMissense_hg38.tsv")
# head(AlphaMissense_data)
# vcf_data[c(20:30),]
# not_snp_data[c(20:30),]
#
# # Use the function
# combined_vcf_data <- readAndCombineVCFs("~/data/Freebayes")
# result <- getPrediction(combined_vcf_data, AlphaMissense_data)
# head(result)
# # Summaries
# pathogenicity_summary <- summarize_pathogenicity(result)
# print(pathogenicity_summary)
# class_summary <- summarize_class(result)
# print(class_summary)
#
# # Plots
# scoreVis_plot <- scoreVis(result)
# print(scoreVis_plot)
#
# classVis_plot <- classVis(result)
# print(classVis_plot)
