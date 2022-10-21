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
      tags$h3("Pandora & IsoMemo Data search with Spatiotemporal modeling"),
      tags$br(),
      tags$h5("To access Pandora & IsoMemo modeling please:"),
      tags$ul(
        tags$li(HTML(
          paste("Visit", getHrefTag("https://pandoraapp.earth/"), "or",
                 getHrefTag("https://isomemoapp.com/"), "[this may be slow].",
                 "There you can select the option Data Search and Spatiotemporal modeling"
          )
        ))
        ),
      tags$h5("or"),
      tags$ul(
        tags$li(HTML(
          paste(
            "Install apps locally. General installation instruction are available at",
            getHrefTag("https://github.com/Pandora-IsoMemo/drat"), ". Installation instructions",
            "for the Data Search and Spatiotemporal Modeling app are available at",
            getHrefTag("https://github.com/Pandora-IsoMemo/drat/issues/3"),
            "."
          )
        ))
      )
    )
  )
}

#' Get Href
#'
#' @param url url
getHrefTag <- function(url) {
  tags$span(
    tags$a(href = url, url)
  )
}

