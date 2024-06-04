.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    "IsoMemo",
    system.file("dist", package = "DSSM")
  )
}
