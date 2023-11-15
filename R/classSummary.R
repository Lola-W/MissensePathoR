#' Summarize Class Distribution by Group
#'
#' This function calculates the count of each class within each group and
#' returns a data table with a summary. It requires the `data.table` package
#' for its operations.
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
  # Ensure 'data' is a data.table, contributed by ChatGPT-4
  if (!inherits(data, "data.table")) {
    stop("The 'data' parameter must be a data.table object.")
  }

  # Ensure the required columns are present
  required_cols <- c("group", "am_class")
  if (!all(required_cols %in% names(data))) {
    stop("The 'data' data.table must contain the 'group' and 'am_class' columns.")
  }

  # Summarize the count by group and am_class
  summary_data <- data[, .(
    count = .N
  ), by = .(group, am_class)]

  # Pivot the data for a more readable format using dcast from data.table
  summary_data <- dcast(summary_data, group ~ am_class, value.var = "count", fill = 0)

  return(summary_data)
}
