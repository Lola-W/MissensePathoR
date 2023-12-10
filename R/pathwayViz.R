#' Visualize Enrichment Pathways
#'
#' Provides dot plots displaying enriched pathways for a given set of genes,
#' enhancing the interpretability of pathway analysis results.
#'
#' @param enrichOut A list containing enrichment results as obtained from
#'   an enrichment analysis function like \code{gost}. This list should contain
#'   elements named 'sig', 'up', and 'down', each with a sub-element 'go_bp'.
#' @param type Character string specifying the type of genes to visualize.
#'   Should be one of 'sig' (significant), 'up' (up-regulated), or 'down' (down-regulated).
#'   Default is 'sig'.
#'
#' @return A ggplot object representing the top 10 enriched pathways for the
#'   specified gene set.
#'
#' @import ggplot2
#' @importFrom stats setNames
#' @export
#'
#' @examples
#' \dontrun{
#'   pathwayViz(enrichOut, type = "sig")
#' }
pathwayViz <- function(enrichOut, type = "sig") {
  # Validate input
  if (!"sig" %in% names(enrichOut) || !"up" %in% names(enrichOut) || !"down" %in% names(enrichOut)) {
    stop("Please consider using enrichSNP() to perform Pathway Enrichment for SNPs. enrichOut must contain 'sig', 'up', and 'down' elements.")
  }
  if (!type %in% c("sig", "up", "down")) {
    stop("Please consider using enrichSNP() to perform Pathway Enrichment for SNPs. Type must be one of 'sig', 'up', or 'down'.")
  }

  # Extract the relevant data based on the type
  if (type == "sig" && !is.null(enrichOut$sig)) {
    enrichment_data <- enrichOut$sig$go_bp
    plot_title <- "Enrichment Analysis on All Significant Top Hits"
  } else if (type == "up" && !is.null(enrichOut$up)) {
    enrichment_data <- enrichOut$up$go_bp
    plot_title <- "Enrichment Analysis on All Up-Regulated Genes"
  } else if (type == "down" && !is.null(enrichOut$down)) {
    enrichment_data <- enrichOut$down$go_bp
    plot_title <- "Enrichment Analysis on All Down-Regulated Genes"
  } else {
    stop(paste("Please consider using enrichSNP() to perform Pathway Enrichment for SNPs. No GO:BP data available for", type, "analysis in enrichOut."))
  }

  # Check if enrichment_data is not empty
  if (nrow(enrichment_data) < 1) {
    stop("No enrichment data found for the selected type.")
  }

  # Create the dot plot
  ggplot(enrichment_data[1:10, ]) +
    geom_point(aes(
      x = precision,
      color = p_value,
      y = term_name,
      size = intersection_size)) +
    theme(axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8)) +
    scale_color_gradient(low = "red", high = "blue") +
    labs(x = "Precision",
         color = "P-value",
         size = "Intersected genes",
         y = NULL,
         title = plot_title)
}
