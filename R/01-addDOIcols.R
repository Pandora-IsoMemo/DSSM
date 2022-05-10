generateDOI <- function(DF,citationtype,citationstyle){
  databaseDOIout <- character()
  for (x in DF$databaseDOI) {
    databaseDOIout <- c(databaseDOIout , cr_cn(doi=x, format =  citationtype,style = citationstyle))
  }
  originalDOIout <- character()
  for (x in DF$originalDataDOI) {
    originalDOIout <- c(originalDOIout , cr_cn(doi=x, format =  citationtype,style = citationstyle))
  }
  compilationDOIout <- character()
  for (x in DF$compilationDOI) {
    compilationDOIout <- c(compilationDOIout , cr_cn(doi=x, format =  citationtype,style = citationstyle))
  }
  DOI <- cbind(databaseDOIout, originalDOIout,compilationDOIout)
  DF <- cbind(DF,DOI)
  return(DF)
}

# notes:
# 03-dataExplorer.R: line 441,
DF = generateDOI(test_df[0:10,],"bibtex","APA") # ran this: only ten rows, the API took about 25 seconds, so 200
