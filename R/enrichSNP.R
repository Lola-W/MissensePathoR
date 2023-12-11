#' Helper function, Perform GOST Analysis
#'
#' This helper function uses the g:Profiler tool for functional mapping and performs a GOST analysis on the provided gene list.
#'
#' @param gene_list A vector of gene identifiers.
#' @param query_type A character string specifying the type of query.
#'
#' @return A list containing the GOST result, a plot of the GOST result, and GO Biological Process results.
#' @importFrom gprofiler2 gost gostplot
#' @export
performGost <- function(gene_list, query_type) {
  # Validate input
  if (length(gene_list) < 1) {
    stop("The gene list is empty.")
  }
  if (!is.character(query_type) || length(query_type) != 1) {
    stop("Query type must be a single character string.")
  }

  # Perform GOST analysis
  gostres <- gprofiler2::gost(query = gene_list,
                              organism = "hsapiens", ordered_query = FALSE,
                              multi_query = FALSE, significant = TRUE, exclude_iea = FALSE,
                              measure_underrepresentation = FALSE, evcodes = FALSE,
                              user_threshold = 0.05, correction_method = "fdr",
                              domain_scope = "annotated", custom_bg = NULL,
                              numeric_ns = "", sources = NULL, as_short_link = FALSE)

  # Update the query type in the result
  gostres$result$query <- query_type

  # Generate the GOST plot (non-interactive mode)
  gost_plot <- gprofiler2::gostplot(gostres, interactive = FALSE)

  # Extract GO Biological Process (GO:BP) results
  gobp <- gostres$result[gostres$result$source == "GO:BP", ]

  # Return a list containing the GOST result, plot, and GO:BP results
  return(list(gost_result = gostres, plot = gost_plot, go_bp = gobp))
}



#' Pathway Enrichment Analysis Using g:Profiler
#'
#' This function performs pathway enrichment analysis on SNP data, identifying
#' up- and down-regulated genes based on thresholded over-representation analysis.
#' It utilizes g:Profiler (Raudvere et al. 2019) for functional mapping.
#'
#' @param diffOut A TopTags object containing differential expression results
#' with columns 'FDR' and 'logFC'.
#'
#' @return A list containing enrichment results for up-regulated, down-regulated,
#' and all significantly thresholded genes.
#' @details
#' The function applies a significance threshold (FDR <= 0.05) to categorize genes
#' into up-regulated and down-regulated groups based on log fold change (logFC).
#' In edge cases where fewer than one gene is identified as significantly
#' differentially expressed, the function adjusts the gene lists accordingly:
#' - If no genes meet the significance criteria, the function stops and issues a warning.
#' - If only a few genes are identified as significant, the function expands the gene list
#'   based on fold changes(logFC) to ensure a minimum number(3) of genes are analyzed.
#' - Special handling is done to ensure a balanced representation of up- and down-regulated genes.
#'
#' Pathway enrichment analysis is conducted using the g:Profiler tool,
#' specifically employing the GOST (gene set over-representation testing) method.
#'
#' @importFrom gprofiler2 gost
#' @export
#' @examples
#' \dontrun{
#'   enrichOut <- enrichSNP(diffOut)
#' }
#' @references
#' Raudvere, Uku, et al. (2019). "G: Profiler: A Web Server for Functional Enrichment
#' Analysis and Conversions of Gene Lists (2019 Update)." Nucleic Acids Research,
#' 47(W1): W191–98.
enrichSNP <-  function(diffOut) {
  # Check if diffOut has the required columns
  requiredCols <- c("FDR", "logFC")
  if (!all(requiredCols %in% names(diffOut$table))) {
    stop("Please consider using diffSNPs() to perform Differential SNP Analysis. diffOut must be a TopTags object contain columns: ", paste(requiredCols, collapse = ", "))
  }

  gobp<- c()
  sig_list <- rownames(diffOut[diffOut$table$FDR <= 0.05,])
  up_list <- rownames(diffOut[diffOut$table$FDR <= 0.05 & diffOut$table$logFC > 1,])
  down_list <- rownames(diffOut[diffOut$table$FDR <= 0.05 & diffOut$table$logFC < -1,])

  # Check edge cases
  # if no genes are significantly differentially expressed
  if (length(sig_list) < 1) {
    stop("All genes are not significant in our pipeline.")
  } else if (length(up_list) <= 1 && length(down_list) <= 1) {
    message("As only ", length(up_list), " up-regulated genes and ", length(down_list),
            " down-regulated genes are identified as significant in our pipeline, ",
            "we threshold select the top ", length(sig_list),
            " genes on fold changes (to align with our total of ", max(3, length(sig_list)),
            " significant genes identified).")
    up_list <- rownames(head(diffOut$table[order(diffOut$table$logFC, decreasing = TRUE),], max(3, length(sig_list))))
    down_list <- rownames(head(diffOut$table[order(diffOut$table$logFC),], max(3, length(sig_list))))
  } else {
    if (length(up_list) <= 1) {
      message("As only ", length(up_list), " up-regulated genes are identified as significant in our pipeline, ",
              "we threshold select the top ", length(down_list),
              " genes on fold changes (to align with our ", length(down_list),
              " significant genes identified for down regulation).")
      up_list <- rownames(head(diffOut$table[order(diffOut$table$logFC, decreasing = TRUE),], length(down_list)))
    }
    if (length(down_list) <= 1) {
      message("As only ", length(down_list), " down-regulated genes are identified as significant in our pipeline, ",
              "we threshold select the top ", length(up_list),
              " genes on fold changes (to align with our ", length(up_list),
              " significant genes identified for up regulation).")
      down_list <- rownames(head(diffOut$table[order(diffOut$table$logFC),], length(up_list)))
    }
  }

  # Perform GOST analysis for each gene set
  up_results <- performGost(up_list, "Up-Regulated Genes")
  down_results <- performGost(down_list, "Down-Regulated Genes")
  sig_results <- performGost(sig_list, "Thresholded Genes")

  return(list(up = up_results, down = down_results, sig = sig_results))
}
