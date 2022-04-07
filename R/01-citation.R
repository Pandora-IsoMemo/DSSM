generateCitation <- function(data, type, file) {
  stopifnot(ncol(data) == 6)
  #browser()
  data <- data[!duplicated(data), ]
  citations <- citationList(data)
  switch(
    type,
    txt = generateCitationTxt(citations, file),
    xml = generateCitationXml(citations, file),
    json = generateCitationJson(citations, file)
  )
}

generateCitationTxt <- function(citations, file) {
  content <- paste(citations, collapse = "\n\n")
  write(content, file)
}

generateCitationXml <- function(citations, file) {
  x <- lapply(seq_along(citations), function(i) paste0("<citation id=\"", i, "\">", citations[i], "</citation>"))
  x <- paste(x, collapse = "\n")
  x <- paste(
    "<?xml version=\"1.0\"?>",
    "<citations>",
    x,
    "</citations>"
  )
  write(x, file)
}

generateCitationJson <- function(citations, file) {
  content <- jsonlite::toJSON(citations, auto_unbox = TRUE)
  write(content, file)
}

citationList <- function(data) {
  apply(data, 1, citationLine)
}

citationLine <- function(row) {
  paste(
    citationElement(row[1], row[2]),
    citationElement(row[3], row[4]),
    citationElement(row[5], row[6]),
    sep = "\n\n"
  )
}

citationElement <- function(ref, doi) {
  paste0(ref, "\nDOI: ", doi)
}

# TODO: how to use this in the console?
#generateRcrossRefromDOI <- function(tmp,style,format){
#  data <- cr_cn(doi, format = format,style = style)
#  return(data)
  #rdf_xml < -cr_cn(dois=doi, format= "rdf-xml")
  #citejson <- cr_cn(dois=doi, format= "citeproc-json")
  #citejson_ish <- cr_cn(dois=doi, format= "citeproc-json-ish")
  #text <- cr_cn(dois=doi, format= "text")
  #ris <- cr_cn(dois=doi, format= "ris")
  #cross_xml <- cr_cn(dois=doi, format= "crossref-xml")
#  #cr_cn(dois=doi, format= "datacite-xml")
  #bibentry <- cr_cn(dois=doi, format= "bibentry")
  #cross_tdm <- cr_cn(dois=doi, format= "crossref-tdm")

# sometimes the DOI is not there: database, original, compilation: when the DOI is empty, the txt reference is generated
  #step 2: harvard vs. chicago style: fetch the text, not using the itsef, but use the DOI
  #cr_cn("10.1126/science.169.3946.635", "text", "apa")
  #cr_cn("10.1126/science.169.3946.635", "text", "harvard3")
  #cr_cn("10.1126/science.169.3946.635", "text", "elsevier-harvard")
  #cr_cn("10.1126/science.169.3946.635", "text", "ecoscience")
  #cr_cn("10.1126/science.169.3946.635", "text", "heredity")
  #cr_cn("10.1126/science.169.3946.635", "text", "oikos")
  #cr_cn('https://doi.org/10.1016/j.jasrep.2017.07.030', format = "bibtex",style = "geophysics")
  # output: have three columns.
  # drop down for citation format:
  # drop down: change citation export type
  #originalDataDOIauto
#}
