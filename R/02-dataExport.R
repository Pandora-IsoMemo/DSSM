dataExportButton <- function(id, title = "Export Data") {
  ns <- NS(id)
  actionButton(ns("export"), title)
}

dataExport <- function(input, output, session, dataFun, filename = "data") {
  observeEvent(input$export, {
    showModal(modalDialog(
      "Export Data",
      easyClose = TRUE,
      footer = modalButton("OK"),
      selectInput(
        session$ns("exportType"),
        "File type",
        choices = c("csv", "xlsx", "json"),
        selected = "xlsx"
      ),
      conditionalPanel(
        condition = "input['exportType'] == 'csv'",
        ns = session$ns,
        div(style = "display: inline-block;horizontal-align:top; width: 80px;",
            textInput(session$ns("colseparator"), "column separator:", value = ",")),
        div(style = "display: inline-block;horizontal-align:top; width: 80px;",
            textInput(session$ns("decseparator"), "decimal separator:", value = "."))
      ),
      downloadButton(session$ns("exportExecute"), "Export")
    ))
  })

  output$exportExecute <- downloadHandler(
    filename = function(){
      exportFilename(filename, input$exportType)
    },
    content = function(file){
      switch(
        input$exportType,
        csv = exportCSV(file, dataFun()(), input$colseparator, input$decseparator),
        xlsx = exportXLSX(file, dataFun()()),
        json = exportJSON(file, dataFun()())
      )
    }
  )
}

#' Filename of Export
#'
#' @param fileending character csv or xlsx
#' @param filename name of file
#'
#' @export
exportFilename <- function(filename = "isotopeData", fileending){
  paste(filename, fileending, sep = ".")
}

#' Export to csv
#'
#' @param file filename
#' @param dat data.frame
#' @param colseparator column seperator
#' @param decseparator decimal seperator
#' @export
exportCSV <- function(file, dat, colseparator, decseparator){
  write.table(x = dat, file = file, sep = colseparator,
              dec = decseparator, row.names = FALSE)
}

#' Export to xlsx
#'
#' @param file filename
#' @param dat data.frame
#' @export
exportXLSX <- function(file, dat){
  write.xlsx(dat, file)
}

#' Export to json
#'
#' @param file filename
#' @param dat data.frame
exportJSON <- function(file, dat){
  json <- toJSON(dat)
  write(json, file)
}
