#' Map all variants to HGNC gene names
#'
#' This function maps AlphaMissense predicted UniProt IDs to HGNC gene names using UniProt.ws and merges this information with the original data.
#'
#' @param predScoreSample A data.frame or data.table with a column 'uniprot_id' containing UniProt IDs.
#'
#' @return A data frame that merges the input data frame with a new column 'hgnc_gene' containing the corresponding HGNC gene names.
#'
#' @importFrom UniProt.ws mapUniProt
#' @import dplyr
#' @export
#'
#' @examples
#' # predScoreSample <- data.frame(uniprot_id = c("P12345", "Q67890"))
#' # result <- mapGene(predScoreSample)
mapGene <- function(predScoreSample) {
  # Check if predScoreSample has the necessary column
  if (!"uniprot_id" %in% colnames(predScoreSample)) {
    stop("predScoreSample must contain the column 'uniprot_id'")
  }

  # Extract unique uniprot_ids from the data
  unique_uniprot_ids <- unique(predScoreSample$uniprot_id)

  # Get mapping for these uniprot_ids using UniProt.ws
  uniprot_mapping <- UniProt.ws::mapUniProt("UniProtKB_AC-ID", "UniProtKB", query = unique_uniprot_ids)
  uniprot_mapping$Gene.Names <- gsub(" .*$", "", uniprot_mapping$Gene.Names) # first gene name

  # Create a mapping from UniProt to HGNC using dplyr
  uniprot_to_hgnc_mapping <- dplyr::mutate(uniprot_mapping,
                                           hgnc_gene = gsub(" .*$", "", Gene.Names)) %>%
    dplyr::select(From, hgnc_gene)

  # Merge the mapping with the original data
  result <- merge(predScoreSample, uniprot_to_hgnc_mapping, by.x = "uniprot_id", by.y = "From", all.x = TRUE)

  return(result)
}
