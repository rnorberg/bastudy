#' @title bagui
#' @description Launches GUI to make using this package really, really easy
#' @details None at this time
#' @aliases bagui
#' @author Jung-han Wang and Robert Norberg
#' @export bagui

bagui <- function() {
  appDir <- system.file("examples", "gui", package = "bastudy")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `bastudy`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
