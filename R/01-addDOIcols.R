generateDOI <- function{
  databaseDOIout <- character()
  for (x in DF$databaseDOI) {
    databaseDOIout <- c(databaseDOIout , cr_cn(doi=x, format =  input$citationtype,style = input$citationstyle))
  }
  originalDOIout <- character()
  for (x in DF$originalDataDOI) {
    originalDOIout <- c(data$originalDataDOI , cr_cn(doi=x, format =  input$citationtype,style = input$citationstyle))
  }
  compilationDOIout <- character()
  for (x in DF$compilationDOI) {
    compilationDOIout <- c(compilationDOIout , cr_cn(doi=x, format =  input$citationtype,style = input$citationstyle))
  }
  DOI <- cbind(databaseDOIout, originalDOIout,compilationDOIout)
  DF <- cbind(DF,DOI)
  DF
}

