library(ggplot2)
# library(tidyverse)
# utils::install.packages("here")
library(dplyr)
library(vcfR)
library(data.table)

usethis::use_package("ggplot2")
use_package("dplyr")
usethis::use_package("vcfR")
usethis::use_package("data.table")

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
                          .(CHROM, POS, REF, ALT, genome, uniprot_id, transcript_id, protein_variant, am_pathogenicity, am_class, QUAL),
                          on = .(CHROM, POS, REF, ALT)]
                          # on = .(CHROM, POS)]

  # Report the number of removed rows
  removed_rows <- nrow(vcf_data) - nrow(merged_data)
  message(paste("Number of removed rows:", removed_rows))

  return(merged_data)
}

#read
vcf_data <- getVCF("/Users/wengjiaming/Downloads/F1S4_160721_071_G01merged_snps.vcf")
vcf_data <- vcf_data%>%
  mutate(CHROM = paste0("chr", CHROM))
AlphaMissense_data <- getAlphaMissenseData("/Users/wengjiaming/Downloads/AlphaMissense_hg38.tsv")
head(AlphaMissense_data)
vcf_data[c(20:30),]

# Use the function
result <- getPrediction(vcf_data, AlphaMissense_data)
table(result$am_class)
