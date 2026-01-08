# ==============================================================================
# SHINY APPLICATION LAUNCHER
# ==============================================================================

#' Launch OPTIVAL Interactive Shiny Application
#' 
#' Launches an interactive web application for performing OPTIVAL analyses
#' through a user-friendly graphical interface.
#' 
#' @details 
#' The Shiny application provides a point-and-click interface for users who
#' prefer not to use R programming directly. Features include:
#' \itemize{
#'   \item Data upload (CSV, TXT, or other delimited formats)
#'   \item Optional precalibrated loadings upload
#'   \item Configurable bootstrap replications
#'   \item Automatic file format detection
#'   \item Interactive plots with zoom and hover information
#'   \item Downloadable results tables
#'   \item Comprehensive summary statistics
#' }
#' 
#' @return No return value. Launches the Shiny application in the default
#'   web browser or RStudio Viewer pane.
#' 
#' @examples
#' \dontrun{
#' # Launch the application
#' library(OPTIVAL)
#' run_optival()
#' }
#' 
#' @export
#' @importFrom shiny runApp
run_optival <- function() {
  app_dir <- system.file("shiny-app/app", package = "OPTIVAL")
  
  if (app_dir == "") {
    stop("Could not find Shiny app directory. Try re-installing OPTIVAL.")
  }
  
  shiny::runApp(app_dir, display.mode = "normal")
}
