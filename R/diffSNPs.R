#' Differential SNP Analysis
#'
#' This function performs differential SNP analysis based on pathogenicity scores and variant frequencies.
#' We used variant frequency as the pseudo gene count and normalized it using CPM (Counts Per Million).
#' Incorporates pathogenicity score for weighted CPM calculation and uses a Quasi-likelihood model to account for over-dispersion.
#'
#'#' @details
#' The function treats variant frequencies as pseudo gene counts, normalized using the Counts Per Million (CPM) method. It also incorporates pathogenicity scores for a weighted CPM calculation, offering a more refined measure of variant expression. The weighted CPM accounts for the predicted pathogenicity, with weights assigned based on AlphaMissense score thresholds.
#'
#' The Quasi-likelihood model from `edgeR` is employed to account for over-dispersion in the data, ensuring robust statistical analysis.
#'
#' @param data A data frame containing 'uniprot_id', 'sample_name', and 'am_class'.
#' @param groupControl The reference level for the 'group' factor in the model design.
#' @return A table of differentially expressed SNPs.
#' @importFrom dplyr mutate select
#' @importFrom edgeR DGEList calcNormFactors estimateDisp glmQLFit glmQLFTest topTags
#' @export

#' @examples
#' \dontrun{
#'   result <- mapGene(predScoreSample)
#'   result$sample_name = paste0(result$sample_name,"_",result$group)
#'   diffOut <- diffSNPs(result, "0h")
#'   head(diffOut$table)
#' }
diffSNPs <- function(data, groupControl) {
  # Validate input
  if (!("group" %in% names(data)) ||
      !("hgnc_gene" %in% names(data)) ||
      !("sample_name" %in% names(data)) ||
      !("am_class" %in% names(data))) {
    stop("Please consider using mapGene() to get HGNC gene names for AlphaMissense predictions. Data must contain 'group', hgnc_gene', 'sample_name', and 'am_class'.")
  }

  # Calculate variant frequency and total count for each sample
  variant_counts <- data[, .N, by = .(sample_name, CHROM, POS, REF, ALT)]
  colnames(variant_counts)[ncol(variant_counts)] <- "variant_freq"
  data <- merge(data, variant_counts, by = c("sample_name", "CHROM", "POS", "REF", "ALT"))
  data <-data %>% mutate(patho_weight = case_when(am_class == "likely_benign" ~ 0.5,
                                           am_class == "ambiguous" ~ 1,
                                           am_class == "likely_pathogenic" ~ 2))
  total_counts <- data[, .(total_count = sum(variant_freq)), by = sample_name]

  #  calculate weighted CPM (including pathogenicity score)
  data <- merge(data, total_counts, by = "sample_name")
  data[, weighted_cpm := (patho_weight * variant_freq / total_count) * 1e6]

  # DE analysis
  # Prepare counts matrix for
  counts_matrix <- dcast(data, hgnc_gene ~ sample_name, value.var = "weighted_cpm",fun.aggregate = sum)
  counts_matrix <- data.frame(counts_matrix, row.names = "hgnc_gene")

  # Design model
  samples <- data %>%  select(group, sample_name) %>% unique()
  samples$group <- relevel(as.factor(samples$group), ref = groupControl)
  model_design <- model.matrix(~  samples$group)
  # Create DGEList object
  dge <- DGEList(counts = counts_matrix, group = samples$group)
  dge <- calcNormFactors(dge)
  # dispersion
  dge <- estimateDisp(dge, model_design)

  # Differential expression analysis
  # Using edgeR
  fit <- glmQLFit(dge, model_design)
  results <- glmQLFTest(fit)

  qlf_output_hits <- topTags(results,sort.by="logFC",
                             n = nrow(counts_matrix))

  return(qlf_output_hits)
}
