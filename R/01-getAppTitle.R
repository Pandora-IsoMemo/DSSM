#' Get App Title
#'
#' If the full app is accessible (if Sys.getenv("ISOMEMO_HIDE_MODELLING") == ""),
#' sets the title of the app to "Data search & Spatiotemporal modeling"; and else to
#' "Data search", the modeling feature is hidden in that case.
#'
#' @export
getAppTitle <- function() {
  if (Sys.getenv("ISOMEMO_HIDE_MODELLING") != "") {
    "Data search"
  } else {
    "Data search & Spatiotemporal modeling"
  }
}
