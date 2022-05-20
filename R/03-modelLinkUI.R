#' ui function to display a download link
#'
#' @param id namespace
#' @param title title in tab
#'
#' @export
modelLinkUI <- function(id, title = "") {
  ns <- NS(id)
  tabPanel(
    title,
    id = id,
    value = id,
    mainPanel(
      tags$h3("Pandora & IsoMemo app with modeling feature"),
      tags$br(),
      tags$h5("To use the modeling options please"),
      tags$ul(tags$li(HTML(
        paste0("[slow option] visit ", tags$span(
          tags$a(href = "https://isomemoapp.com/app/iso-memo-app",
                 "https://isomemoapp.com/app/iso-memo-app")
        ), tags$br(), "or")
      )),
      tags$li(HTML(
        paste0(
          "[fast option] install this app locally by following the instructions given on GitHub:",
          tags$br(),
          "app installation: ",
          tags$span(
            tags$a(href = "https://github.com/Pandora-IsoMemo/drat/issues/3",
                   "https://github.com/Pandora-IsoMemo/drat/issues/3")
          ),
          " and",
          tags$br(),
          "general instructions: ",
          tags$span(
            tags$a(href = "https://github.com/Pandora-IsoMemo/drat",
                   "https://github.com/Pandora-IsoMemo/drat")
          )
        )
      )))
    )
  )
}
