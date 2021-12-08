#' @title Shiny demo
#' @description Run a Shiny app which demonstrates the Kochanek-Bartels spline.
#'
#' @return No value returned.
#' @export
#' @importFrom shiny shinyAppDir
shinyDemo <- function(){
  appDir <- system.file("shiny", "threejs", package = "onion")
  shinyAppDir(appDir, options = list(launch.browser = TRUE))
}