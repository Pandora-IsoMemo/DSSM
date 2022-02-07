#' Check Availability for ReSources package
#'
#' @export
reSourcesInstalled <- function() {
  "ReSources" %in% installed.packages()[, 1] &&
    compareVersion(as.character(packageVersion("ReSources")), reSourcesVersion()) > -1
}

reSourcesVersion <- function() { "0.7.9" }
