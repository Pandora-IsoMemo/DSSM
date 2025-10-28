# Map CSL-like style ids to RefManageR .opts lists
refmanager_style_opts <- function(
  style = c("apa", "chicago", "harvard"), # "chicago-author-year", "harvard-cite-them-right"
  format = c(
    "text",
    "Bibtex",
    "Biblatex",
    "citation",
    "html",
    "latex",
    "markdown",
    "yaml",
    "R"
  )
) {
  style <- match.arg(style)
  format <- match.arg(format)
  switch(tolower(style),
    # APA 7-ish
    "apa" = list(
      bib.style       = "authoryear",
      max.names       = 100,
      first.inits     = TRUE,
      dashed          = FALSE,
      sorting         = "nyt",  # sort by name, year, title
      no.print.fields = c("ISSN", "publisher", "month", "doi"),
      style = format
    ),
    # Chicago Manual of Style 17e (author-date) -ish
    "chicago" = list(
      bib.style       = "authoryear",
      max.names       = 10,
      first.inits     = FALSE,
      dashed          = TRUE,   # 3-em dash for repeated authors in bib
      sorting         = "nyt",  # sort by name, year, title
      no.print.fields = c("ISSN", "publisher", "doi"),
      style = format
    ),
    # Harvard (Cite Them Right) -ish
    "harvard" = list(
      bib.style       = "authoryear",
      max.names       = 1,
      first.inits     = TRUE,
      dashed          = FALSE,
      sorting         = "nyt",  # sort by name, year, title
      no.print.fields = c("ISSN", "publisher", "month", "doi"),
      style = format
    ),
    # default if unknown style id
    list()
  )
}

convert_bibentry_format <- function(bib, style_opts) {
  # convert single bibentry to desired format
  out <- paste(capture.output(print(bib, .opts = style_opts)), collapse = " ")
  out <- gsub("\\s+", " ", out) # collapse multiple spaces
  out
}

read_bib_from_text <- function(bibtex_vec) {
  bib_str <- paste(bibtex_vec, collapse = "\n")
  tf <- tempfile(fileext = ".bib")
  on.exit(unlink(tf), add = TRUE)
  writeLines(bib_str, tf)
  RefManageR::ReadBib(tf)
}

get_supported_citation_formats <- function() {
  c(
    "text",
    "Bibtex",
    "Biblatex",
    "citation",
    "html",
    "latex",
    "markdown",
    "yaml",
    "R"
  )
}

get_supported_citation_styles <- function() {
  c("apa", "chicago", "harvard")
}

# formats from crossref:
#   "rdf-xml", "turtle", "citeproc-json", "citeproc-json-ish", "text", "ris", "bibtex" (default),
#   "crossref-xml", "datacite-xml","bibentry", or "crossref-tdm".
format_bibtex_citations <- function(
  bibtex_vec,
  style_opts,
  colname = ""
) {
  # get unique bibtex entries
  bibtex_unique <- unique(bibtex_vec)
  # get rid of NA and empty entries
  bibtex_unique <- bibtex_unique[!is.na(bibtex_unique) & bibtex_unique != ""]

  # if no valid entries, return NA vector
  if (length(bibtex_unique) == 0) {
    return(rep(NA_character_, length(bibtex_vec)))
  }

  # read bibtex entries
  tryCatch(
    {
      bib_list <- read_bib_from_text(bibtex_unique)
    },
    error = function(cond) {
      if (colname != "") colname <- sprintf(" '%s'", colname)
      logWarn("Could not read bibtex column%s. Error:", colname, cond$message)
      # Choose a return value in case of error
      rep(NA_character_, length(bibtex_vec))
    },
    finally = rep(NA_character_, length(bibtex_vec))
  )

  # break if all values are NA
  if (all(is.na(bib_list))) {
    return(rep(NA_character_, length(bibtex_vec)))
  }

  # format each entry
  bib <- lapply(bib_list, function(entry) {
    tryCatch(
      {
        convert_bibentry_format(entry, style_opts = style_opts)
      },
      error = function(cond) {
        logWarn("Could not convert from bibtex entry! Error: %s", cond$message)
        # Choose a return value in case of error
        NA_character_
      },
      finally = NA_character_
    )
  })

  # create citation vector
  citation <- vapply(bib, paste, collapse = "\n", character(1), USE.NAMES = TRUE)

  # merge back to original order
  orig <- data.frame(
    bibtex = bibtex_vec,
    stringsAsFactors = FALSE
  )

  df <- data.frame(
    bibtex = bibtex_unique,
    citation = citation,
    stringsAsFactors = FALSE
  )

  merged <- merge(
    orig,
    df,
    by = "bibtex",
    all.x = TRUE,
    sort = FALSE
  )

  # return citation vector
  return(merged$citation)
}

