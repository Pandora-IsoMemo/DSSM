generateDOI <- function(DF,citationtype,citationstyle){
  # if there are missing values, then skip the row. and the end return the same index.
  #library(rcrossref)
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

  DOI <- cbind(databaseDOIout, originalDOIout,compilationDOIout)
  DF <- cbind(DF,DOI)
  return(databaseDOIout)
}

DF = test_df[1:10,]

# notes:
# 03-dataExplorer.R: line 441,
a <- Sys.time()
generateDOI(DF,"bibtex","APA") # ran this: only ten rows, the API took about 25 seconds, so 200
#cr_cn(doi=DF$databaseDOI, "bibtex","APA")
Sys.time() - a
