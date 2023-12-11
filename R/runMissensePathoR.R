#' Launch Shiny App for MissensePathoR
#'
#' A function that launches the Shiny app for MissensePathoR. This app provides
#' a user interface to interactively work with the MissensePathoR package.
#' The app's code is located in \code{./inst/shiny-scripts}.
#'
#' @return No return value, but opens a Shiny page.
#'
#' @examples
#' \dontrun{
#' MissensePathoR::runMissensePathoR()
#' }
#'
#' @references
#' Grolemund, G. (2015). Learn Shiny - Video Tutorials. \href{https://shiny.rstudio.com/tutorial/}{Link}
#'
#' @export
#' @importFrom shiny runApp

runMissensePathoR <- function() {
  appDir <- system.file("shiny-scripts", package = "MissensePathoR")
  shiny::runApp(appDir, display.mode = "normal")
}
