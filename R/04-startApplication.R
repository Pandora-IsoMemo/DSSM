#' Start Application
#'
#' @param port port
#'
#' @export
startApplication <- function(port = 4242) {
  runApp(
    system.file("app", package = "MpiIsoApp"),
    port = port,
    host = "0.0.0.0"
  )
}
