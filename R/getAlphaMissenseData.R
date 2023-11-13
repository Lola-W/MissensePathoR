#' Get AlphaMissense Data
#'
#' Retrieves AlphaMissense data for analysis. If no path is provided,
#' a demo prediction dataset from AlphaMissense is loaded. For the full dataset,
#' please download it from the provided URL and specify the path to the dataset
#' in the function.
#'
#' @param path The path to the dataset file.
#' If NULL, the function loads a demo dataset included in the package.
#' @return A data.table containing the AlphaMissense data.
#' @importFrom data.table fread
#' @importFrom utils data
#' @importFrom utils warning
#' @examples
#' \dontrun{
#'  # Load the demo dataset
#'  demo_data <- getAlphaMissenseData()
#'
#'  # Load the full dataset (specify the correct path to the downloaded file)
#'  # full_data <- getAlphaMissenseData("path/to/AlphaMissense_hg38.tsv.gz")
#' }
#' @export
getAlphaMissenseData <- function(path = NULL){
  if (is.null(path)) {
    warning("You have loaded a demo prediction dataset from AlphaMissense. ",
            "If you want the full dataset please download it from ",
            "https://storage.googleapis.com/dm_alphamissense/AlphaMissense_hg38.tsv.gz ",
            "and specify the path to the dataset in the function.")
    data("AlphaMissenseSample", package = "MissensePathoR")
    return(AlphaMissenseSample)
  } else {
    AlphaMissense_data <- data.table::fread(path, sep = "\t")
    # Ensure the general format of vcf files
    if ("#CHROM" %in% names(AlphaMissense_data)) {
      data.table::setnames(AlphaMissense_data, "#CHROM", "CHROM")
    }
    return(AlphaMissense_data)
  }
}
