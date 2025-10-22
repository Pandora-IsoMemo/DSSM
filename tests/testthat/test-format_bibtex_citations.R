context("format_bibtex_citations")

bib_db <- "@article{Salesse_2018, title={IsoArcH.eu: An open-access and collaborative isotope database for bioarchaeological samples from the Graeco-Roman world and its margins}, volume={19}, ISSN={2352-409X}, url={http://dx.doi.org/10.1016/j.jasrep.2017.07.030}, DOI={10.1016/j.jasrep.2017.07.030}, journal={Journal of Archaeological Science: Reports}, publisher={Elsevier BV}, author={Salesse, Kevin and Fernandes, Ricardo and de Rochefort, Xavier and Brůžek, Jaroslav and Castex, Dominique and Dufour, Élise}, year={2018}, month=jun, pages={1050–1055} }"

# get bibtex for crossref example
# test_doi <- "10.1016/j.jasrep.2017.07.030"
# rcrossref_bibtex <- rcrossref::cr_cn(test_doi, format = "bibtex")
# print(rcrossref_bibtex)
# print(bib_db)

bib_test1 <- "@article{smith2020, author={Smith, John}, title={A Study}, journal={J}, year={2020}}"
bib_test2 <- "@article{may2023, author={May, Claire}, title={A Study}, journal={J}, year={2023}}"
bibs <- c(bib_test1, bib_test2, bib_db)

# Vector input
test_that("vector input", {
  res <- format_bibtex_citations(bibs, format = "html", style = "chicago")
  expect_equal(
    length(res),
    3
  )
})

# single NA vector input
test_that("NA vector input", {
  res <- format_bibtex_citations(c(NA, bib_test1), format = "html", style = "chicago")
  expect_equal(
    length(res),
    2
  )
})

# all NA vector input
test_that("NA vector input", {
  res <- format_bibtex_citations(c(NA, NA), format = "html", style = "chicago")
  expect_equal(
    res,
    c(NA_character_, NA_character_)
  )
})

# TEXT format
test_that("text format, apa style", {
  res <- format_bibtex_citations(bibs, format = "text", style = "apa")
  # print(rcrossref::cr_cn(test_doi, format = "text", style = "apa"))
  # print(res[[3]])
  expect_equal(
    substr(res[[3]], 1, 80),
    "Salesse, K., R. Fernandes, X. de Rochefort, J. Brůžek, D. Castex, and É. Dufour "
  )
})


test_that("text format, chicago style", {
  res <- format_bibtex_citations(bibs, format = "text", style = "chicago")
  # print(rcrossref::cr_cn(test_doi, format = "text", style = "chicago-author-date"))
  # print(res[[3]])
  expect_equal(
    substr(res[[3]], 1, 80),
    "Salesse, Kevin, Ricardo Fernandes, Xavier de Rochefort, Jaroslav Brůžek, Dominiq"
  )
})


test_that("text format, harvard style", {
  res <- format_bibtex_citations(bibs, format = "text", style = "harvard")
  # print(rcrossref::cr_cn(
  #   test_doi,
  #   format = "text",
  #   style = "harvard-cite-them-right"
  # ))
  # print(res[[3]])
  expect_equal(
    substr(res[[3]], 1, 80),
    "Salesse, K. et al. (2018). “IsoArcH.eu: An open-access and collaborative isotope"
  )
})


# BIBTEX format
test_that("Bibtex format, all styles", {
  for (style in c("apa", "chicago", "harvard")) {
    res <- format_bibtex_citations(bibs, format = "Bibtex", style = style)
    expect_equal(
      substr(res[[3]], 1, 80),
      "@article{Salesse_2018, title = {IsoArcH.eu: An open-access and collaborative iso"
    )
  }
})


# BIBLATEX format
test_that("Biblatex format, all styles", {
  for (style in c("apa", "chicago", "harvard")) {
    res <- format_bibtex_citations(bibs, format = "Biblatex", style = style)
    expect_equal(
      substr(res[[3]], 1, 80),
      "@Article{Salesse_2018, title = {IsoArcH.eu: An open-access and collaborative iso"
    )
  }
})

# CITATION format
test_that("citation format, all styles", {
  exp_res <- c(
    "apa" = " Salesse, K., R. Fernandes, X. de Rochefort, J. Brůžek, D. Castex, and É. Dufour",
    "chicago" = " Salesse, Kevin, Ricardo Fernandes, Xavier de Rochefort, Jaroslav Brůžek, Domini",
    "harvard" = " Salesse, K. et al. (2018). “IsoArcH.eu: An open-access and collaborative isotop"
  )
  for (style in c("apa", "chicago", "harvard")) {
    res <- format_bibtex_citations(bibs, format = "citation", style = style)
    expect_equal(
      substr(res[[3]], 1, 80),
      exp_res[[style]]
    )
  }
})

test_that("html format, all styles", {
  exp_res <- c(
    "apa" = "<p><cite>Salesse, K., R. Fernandes, X. de Rochefort, J. Brůžek, D. Castex, and É",
    "chicago" = "<p><cite>Salesse, Kevin, Ricardo Fernandes, Xavier de Rochefort, Jaroslav Brůžek",
    "harvard" = "<p><cite>Salesse, K. et al. (2018). &ldquo;IsoArcH.eu: An open-access and collab"
  )
  for (style in c("apa", "chicago", "harvard")) {
    res <- format_bibtex_citations(bibs, format = "html", style = style)
    expect_equal(
      substr(res[[3]], 1, 80),
      exp_res[[style]]
    )
  }
})

test_that("latex format, all styles", {
  exp_res <- c(
    "apa" = "Salesse, K., R. Fernandes, X. de Rochefort, J. Brůžek, D. Castex, and É. Dufour ",
    "chicago" = "Salesse, Kevin, Ricardo Fernandes, Xavier de Rochefort, Jaroslav Brůžek, Dominiq",
    "harvard" = "Salesse, K. et al. (2018). ``IsoArcH.eu: An open-access and collaborative isotop"
  )
  for (style in c("apa", "chicago", "harvard")) {
    res <- format_bibtex_citations(bibs, format = "latex", style = style)
    expect_equal(
      substr(res[[3]], 1, 80),
      exp_res[[style]]
    )
  }
})

test_that("markdown format, all styles", {
  exp_res <- c(
    "apa" = "Salesse, K., R. Fernandes, X. de Rochefort, J. Brůžek, D. Castex, and É. Dufour ",
    "chicago" = "Salesse, Kevin, Ricardo Fernandes, Xavier de Rochefort, Jaroslav Brůžek, Dominiq",
    "harvard" = "Salesse, K. et al. (2018). “IsoArcH.eu: An open-access and collaborative isotope"
  )
  for (style in c("apa", "chicago", "harvard")) {
    res <- format_bibtex_citations(bibs, format = "markdown", style = style)
    expect_equal(
      substr(res[[3]], 1, 80),
      exp_res[[style]]
    )
  }
})

test_that("yaml format, all styles", {
  for (style in c("apa", "chicago", "harvard")) {
    res <- format_bibtex_citations(bibs, format = "yaml", style = style)
    expect_equal(
      substr(res[[3]], 1, 80),
      "- type: \"Article\" id: \"Salesse_2018\" title: \"IsoArcH.eu: An open-access and coll"
    )
  }
})

test_that("R format, all styles", {
  for (style in c("apa", "chicago", "harvard")) {
    res <- format_bibtex_citations(bibs, format = "R", style = style)
    expect_equal(
      substr(res[[3]], 1, 80),
      "bibentry(bibtype = \"Article\", key = \"Salesse_2018\", title = \"IsoArcH.eu: An open"
    )
  }
})
