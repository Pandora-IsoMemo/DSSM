#' Is Only Data SEARCH
#'
#' Checks if "only-data-search mode" is active.
#' @return (logical) TRUE if "only-data-search mode" is active, else FALSE for full version of the app.
#' @export
isOnlyDataSearchMode <- function() {
  Sys.getenv("ISOMEMO_HIDE_MODELLING") != ""
}

#' Get App Title
#'
#' If the full app is accessible (if Sys.getenv("ISOMEMO_HIDE_MODELLING") == ""),
#' sets the title of the app to "Data search & Spatiotemporal modeling"; and else to
#' "Data search", the modeling feature is hidden in that case.
#'
#' @export
getAppTitle <- function() {
  if (isOnlyDataSearchMode()) {
    "Pandora & IsoMemo Data search"
  } else {
    "Pandora & IsoMemo Data search and Spatiotemporal modeling"
  }
}

