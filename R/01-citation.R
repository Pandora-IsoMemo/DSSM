get_citation_columns <- function() {
  c(
    "databaseReference",
    "databaseDOI",
    "databaseBibtex",
    "compilationReference",
    "compilationDOI",
    "compilationBibtex",
    "originalDataReference",
    "originalDataDOI",
    "originalDataBibtex"
  )
}

generateCitation <- function(data, type, style, format, file) {
  stopifnot(ncol(data) == 9)
  
  data <- data[!duplicated(data), ]

  withProgress(message = "Generating citation...", value = 0, {
    # apply format to all three bibtex columns
    bibtex_cols <- c("databaseBibtex", "compilationBibtex", "originalDataBibtex")
    for (col in bibtex_cols) {
      new_name <- gsub("Bibtex", "Citation", col)
      logDebug("generateCitation(): Update %s", new_name)
      data[[new_name]] <- format_bibtex_citations(data[[col]], format = format, style = style)
      # inc progress
      incProgress(1 / length(bibtex_cols))
    }
  })
  
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
    citationElement(row[1], row[2], row[3]),
    citationElement(row[4], row[5], row[6]),
    citationElement(row[7], row[8], row[9]),
    sep = "\n\n"
  )
}

citationElement <- function(ref, doi, formatted_citation) {
  paste0(ref, "\nDOI: ", doi, "\nCitation: ", formatted_citation)
}
