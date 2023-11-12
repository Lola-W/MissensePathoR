# library(testthat)
#
# aValue <- 10
# expect_equal(aValue, 10)
# expect_identical(aValue, 10) #also check type
#
# library("biomaRt")
# ensembl<-  useMart("ensembl", dataset="hsapiens_gene_ensembl")
#
# subset_data <- AlphaMissense_data[CHROM == "chr22" & POS <= 20139195, ]
# head(subset_data)
#
# values<- c("NM_013373.4")
#
# getBM(attributes=c("refseq_mrna", "ensembl_gene_id", "hgnc_symbol"), filters = "refseq_mrna", values = values, mart= ensembl)
# aa <- fread("~/Documents/TripLab/Data/AlphaMissense_aa_substitutions.tsv", sep = "\t")
# aa[uniprot_id == "Q9ULC8" & protein_variant == "R709K", ]
# aa[uniprot_id == "Q9ULC8" & protein_variant == "S399R", ]
# aa[uniprot_id == "Q9ULC8" & protein_variant == "L435F", ]
