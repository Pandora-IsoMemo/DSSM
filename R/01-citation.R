generateCitation <- function(data, type, file, citation_columns, style_opts) {
  data <- data[, unlist(citation_columns), drop = FALSE]
  data <- data[!duplicated(data), , drop = FALSE]

  data <- format_bibtex_citations(
    data,
    bibtex_cols = citation_columns$bibtex_cols,
    style_opts = style_opts
  )

  citations <- citationList(data, citation_columns = citation_columns)
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

citationList <- function(data, citation_columns) {
  apply(data, 1, citationLine, citation_columns = citation_columns)
}

citationLine <- function(row, citation_columns) {
  ref_cols <- citation_columns$reference_cols
  doi_cols <- citation_columns$doi_cols
  bib_cols <- citation_columns$bibtex_cols

  n_elements <- max(length(ref_cols), length(doi_cols), length(bib_cols))
  with_ref <- length(ref_cols) > 0
  with_doi <- length(doi_cols) > 0
  with_cite <- length(bib_cols) > 0

  citation_elements <- sapply(seq_len(n_elements), function(i) {
    if (with_ref) ref <- row[ref_cols[i]] else ref <- NULL
    if (with_doi) doi <- row[doi_cols[i]] else doi <- NULL
    if (with_cite) bib <- row[bib_cols[i]] else bib <- NULL
    citationElement(ref, doi, bib, with_ref = with_ref, with_doi = with_doi, with_cite = with_cite)
  })
  paste(citation_elements, collapse = "\n\n")
}

citationElement <- function(ref, doi, cite, with_ref = TRUE, with_doi = TRUE, with_cite = TRUE) {
  if (is.null(ref) || is.na(ref) || ref == "") ref <- "NA"
  if (is.null(doi) || is.na(doi) || doi == "") doi <- "NA"
  if (is.null(cite) || is.na(cite) || cite == "") cite <- "NA"

  ref_str <- if (with_ref) ref else NULL
  doi_str <- if (with_doi) paste0("DOI: ", doi) else NULL
  cite_str <- if (with_cite) paste0("Citation: ", cite) else NULL

  paste(c(ref_str, doi_str, cite_str), collapse = "\n")
}
