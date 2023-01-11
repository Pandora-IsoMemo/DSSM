#' Start Application
#'
#' @param port port
#' @param launch.browser (logical) If true, the system's default web browser will be launched
#'
#' @export
startApplication <- function(port = 4242,
                             launch.browser = getOption("shiny.launch.browser", interactive())) {
  runApp(
    system.file("app", package = "MpiIsoApp"),
    port = port,
    host = "0.0.0.0",
    launch.browser = launch.browser
  )
}
