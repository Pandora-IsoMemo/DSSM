generateCitation <- function(data, type, file) {
  stopifnot(ncol(data) == 6)
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
