#' Visualize Proportions of Pathogenicity Classes
#'
#' This function creates a stacked barplot to visualize the proportions
#' of each pathogenicity class within and between specified groups.
#'
#' @param data A data table containing at least 'group', 'am_class' columns.
#' @param groups A character vector specifying the groups to include in the plot.
#'               If NULL, all groups in 'data' are used.
#' @return A ggplot object representing the stacked barplot of pathogenicity class distribution.
#' @import ggplot2
#' @import RColorBrewer
#' @import data.table
#' @export
#' @examples
#' \dontrun{
#'   classVis(predScoreSample, groups = c("0h", "1h"))
#' }
classVis <- function(data, groups = NULL) {
  if (!inherits(data, "data.table")) {
    stop("The input 'data' must be a data.table object.")
  }
  if (!("am_class" %in% names(data))) {
    stop("'am_class' column not found in the data table.")
  }
  if (!("group" %in% names(data))) {
    stop("'group' column not found in the data table.")
  }
  if (!is.null(groups) && !all(groups %in% data$group)) {
    stop("Not all specified groups are present in the 'group' column of the data table.")
  }
  if (!is.null(groups)) {
    data <- data[group %in% groups]
  }
  
  data$am_class <- factor(data$am_class, levels = c("likely_pathogenic", "ambiguous", "likely_benign"))
  summarized_data <- data[, .(count = .N), by = .(group, am_class)]
  
  ggplot2::ggplot(summarized_data, aes(x = group, y = count, fill = am_class)) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    theme_cowplot() +
    labs(title = "Distribution of Pathogenicity Classes Across Groups",
         y = "Pathogenicity Class", x = "Group") +
    scale_fill_manual(values = c("likely_pathogenic" = "#ed1e24",
                                 "ambiguous" = "#a8a9ad",
                                 "likely_benign" = "#3853a4"),
                      name = "Pathogenicity Class Category") +
    theme(legend.position = "right",
          legend.justification = c(1, 0),
          legend.background = element_rect(fill = "white", colour = "black"),
          legend.key = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          axis.title = element_text(face = "bold", size = 14),
          axis.text = element_text(size = 12))
}
