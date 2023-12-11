#' Summarize Class Distribution by Group
#'
#' This function calculates the count of each class within each group and
#' returns a data table with a summary.
#'
#' @param data A data table object with at least two columns: `group` and `am_class`.
#' @return A data table with groups as rows, classes as columns, and counts as cell values.
#' @import data.table
#' @examples
#' data(predScoreSample)
#' summary <- classSummary(predScoreSample)
#' print(summary)
#'
#' @export
classSummary <- function(data) {
  # Ensure the required columns are present
  required_cols <- c("group", "am_class")
  if (!all(required_cols %in% names(data))) {
    stop("The 'data' must contain the 'group' and 'am_class' columns.")
  }

  data$group = as.character(data$group)

  # Summarize the count by group and am_class
  summary_data <- data %>%
    dplyr::count(as.character(group), am_class) %>%
    tidyr::pivot_wider(names_from = am_class, values_from = n, values_fill = list(n = 0))

  colnames(summary_data)[1] <- "group"

  return(summary_data)
}
