#' Visualize Distribution of Pathogenicity Scores
#'
#' This function creates box plots to visualize the distribution of pathogenicity scores within
#' and between specified groups.
#'
#' @param data A data table containing 'am_pathogenicity' and 'group' columns.
#' @param groups A character vector specifying the groups to include in the plot.
#'               If NULL, all groups in 'data' are used.
#' @return A ggplot object representing the box plots of pathogenicity score distribution.
#' @import ggplot2
#' @import data.table
#' @export
#' @examples
#' \dontrun{
#'   scoreVis(predScoreSample, groups = c("0h", "1h"))
#' }
scoreVis <- function(data, groups = NULL) {
  # Validate input data, contributed by ChatGPT-4
  if (!inherits(data, "data.table")) {
    stop("The input 'data' must be a data.table object.")
  }
  if (!("am_pathogenicity" %in% names(data))) {
    stop("'am_pathogenicity' column not found in the data table.")
  }
  if (!("group" %in% names(data))) {
    stop("'group' column not found in the data table.")
  }
  if (!is.null(groups) && !all(groups %in% data$group)) {
    stop("Not all specified groups are present in the 'group' column of the data table.")
  }

  # Filter data for the specified groups if provided
  if (!is.null(groups)) {
    data <- data[group %in% groups, ]
  }

  # Determine fill colors based on the unique groups present
  unique_groups <- unique(data$group)

  # color
  color_count <- max(3, length(unique_groups)) # Ensure at least 3
  # Get colors from the Set1 palette
  colors <- RColorBrewer::brewer.pal(color_count, "Set1")
  # If there are fewer than 3 groups, repeat some colors
  if (length(unique_groups) < color_count) {
    colors <- colors[1:length(unique_groups)]
  }

  # Plot
  plot <- ggplot(data, aes(x = group, y = am_pathogenicity, fill = group)) +
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
    scale_fill_manual(values = colors, name = "Group")

  return(plot)
}
