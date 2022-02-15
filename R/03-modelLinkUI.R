#' ui function to display a download link
#'
#' @param id namespace
#' @param title title in tab
#'
#' @export
modelLinkUI <- function(id, title = ""){

  ns <- NS(id)
  tabPanel(
    title,
    id = id,
    value = id,
    sidebarLayout(
      sidebarPanel(width = 3,
                   tags$h5("Download IsoMemo app with modeling feature as R-Package."),
                   div(
                     id = "download-full-package",
                     tags$button(
                       onclick = "window.open('https://isomemodb.com/packages/MpiIsoApp.tar.gz','_blank');",
                       class = "btn btn-default",
                       "Download")
                   )),
      mainPanel()
    )
  )
}
