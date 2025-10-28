citationUI <- function(ns) {
  ns <- NS(ns)
  tagList(
    radioButtons(
      ns("citationTemplate"),
      "Citation template",
      choices = c("apa", "chicago", "harvard"),
      selected = "chicago",
      inline = TRUE
    ),
    selectInput(
      ns("bib.style"),
      "Citation style",
      choices = c(
        "Author-year (author and year label, e.g. Smith 2020)" = "authoryear",
        "Numeric (number label, e.g. [1])" = "numeric",
        "Author-title (no label, author and title shown)" = "authortitle",
        "Alphabetic (alphabetic label, e.g. SMI20)" = "alphabetic",
        "Draft (shows entry keys, for draft/review only)" = "draft"
      ),
      selected = "authoryear"
    ),
    numericInput(
      ns("max.names"),
      "Max. number of names before 'et al.'",
      min = 1,
      value = 10,
    ),
    checkboxInput(ns("first.inits"), "First names as initials", value = TRUE),
    checkboxInput(ns("dashed"), "Replace recurring author with dash", value = FALSE),
    selectInput(
      ns("sorting"),
      "Sorting",
      choices = c(
        "name-year-title" = "nyt",
        "name-title-year" = "nty",
        "name-year-volume-title" = "nyvt",
        "alphabetic label-name-year-title" = "anyt",
        "alphabetic label-name-year-volume-title" = "anyvt",
        "year-name-title" = "ynt",
        "year (descending)-name-title" = "ydnt",
        "none" = "none",
        "keys" = "debug"
      )
    ),
    selectInput(
      ns("no.print.fields"),
      "Fields not to print",
      choices = c("ISSN", "publisher", "month", "doi", "url", "volume", "number", "pages"),
      multiple = TRUE,
      selected = c("ISSN", "publisher", "month", "doi")
    ),
    selectInput(
      ns("style"),
      "Printing style",
      choices = get_supported_citation_formats()
    ),
  )
}

citationServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # use default styles
    observe({
      style_opts <- refmanager_style_opts(
        style = input$citationTemplate,
        format = input$style
      )
      updateSelectInput(session, "bib.style", selected = style_opts$bib.style)
      updateNumericInput(session, "max.names", value = style_opts$max.names)
      updateCheckboxInput(session, "first.inits", value = style_opts$first.inits)
      updateCheckboxInput(session, "dashed", value = style_opts$dashed)
      updateSelectInput(session, "sorting", selected = style_opts$sorting)
      updateSelectInput(session, "no.print.fields", selected = style_opts$no.print.fields)
    }) %>% bindEvent(input$citationTemplate)

    # pass user set styles
    citation_options <- reactive({
      list(
        bib.style       = input$bib.style,
        max.names       = input$max.names,
        first.inits     = input$first.inits,
        dashed          = input$dashed,
        sorting         = input$sorting,
        no.print.fields = input$no.print.fields,
        style = input$style
      )
    })

    citation_options
  })
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
