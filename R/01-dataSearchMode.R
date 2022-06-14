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
  subTitle0 <- "" #"Pandora & IsoMemo &ensp;"
  subTitle1 <- "Data search"
  version <- packageVersion("MpiIsoApp")

  if (isOnlyDataSearchMode()) {
    title <- paste(subTitle0, subTitle1, version)
  } else {
    subTitle2 <- "and Spatiotemporal modeling"
    title <- paste(subTitle0, subTitle1, subTitle2, version)
  }

  HTML(title)
}
