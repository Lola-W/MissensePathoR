#' Read AlphaMissense Data
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
#' @examples
#' \dontrun{
#'  # Load the demo dataset
#'  demo_data <- readAlphaMissenseData()
#'
#'  # Load the full dataset (specify the correct path to the downloaded file)
#'  # full_data <- readAlphaMissenseData("path/to/AlphaMissense_hg38.tsv.gz")
#' }
#' @export
readAlphaMissenseData <- function(path = NULL){
  if (is.null(path)) {
    # contributed by ChatGPT-4
    warning("You have loaded a demo prediction dataset from AlphaMissense. ",
            "If you want the full dataset please download it from ",
            "https://storage.googleapis.com/dm_alphamissense/AlphaMissense_hg38.tsv.gz ",
            "unzip with gunzip and specify the path to the dataset in the function.")
    AlphaMissenseSample <- readRDS(system.file("extdata", "AlphaMissenseSample.rda", package = "MissensePathoR"))
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
