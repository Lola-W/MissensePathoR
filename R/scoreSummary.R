#' Calculate Summary Pathogenicity Scores by Specified Category
#'
#' This function calculates various summary statistics of the pathogenicity scores
#' within the specified category of the data.
#'
#' @param data A data table containing pathogenicity scores and categories.
#' @param category A character string specifying the column name to group by.
#' @return A data table with mean, median, minimum, maximum, and standard deviation
#' of the pathogenicity scores for each level of the specified category.
#' @export
#' @examples
#' \dontrun{
#'   scoreSummary(predScoreSample, category = "group")
#'   scoreSummary(predScoreSample, category = "sample_name")
#' }
#' @importFrom data.table data.table
scoreSummary <- function(data, category) {
  # Check if 'data' is a data.table
  if (!inherits(data, "data.table")) {
    stop("The input 'data' must be a data.table object.")
  }

  # Check if 'category' is a character string and exists in the columns of 'data'
  if (!is.character(category) || length(category) != 1) {
    stop("The 'category' argument must be a single character string specifying the column name.")
  }
  if (!(category %in% names(data))) {
    stop(paste("Column", sQuote(category), "not found in the data table."))
  }

  # Check if 'am_pathogenicity' column exists in 'data'
  if (!("am_pathogenicity" %in% names(data))) {
    stop("'am_pathogenicity' column not found in the data table.")
  }

  # Calculate summary statistics grouped by the specified 'category'
  summary_data <- data[, .(
    mean_pathogenicity = mean(am_pathogenicity, na.rm = TRUE),
    median_pathogenicity = median(am_pathogenicity, na.rm = TRUE),
    min_pathogenicity = min(am_pathogenicity, na.rm = TRUE),
    max_pathogenicity = max(am_pathogenicity, na.rm = TRUE),
    sd_pathogenicity = sd(am_pathogenicity, na.rm = TRUE)
  ), by = .(get(category))]  # Dynamic column name for grouping

  setnames(summary_data, old = "get", new = category) # fix the name
  return(summary_data)
}
