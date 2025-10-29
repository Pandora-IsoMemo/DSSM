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

citationStyleUI <- function(ns) {
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

citationStyleServer <- function(id) {
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
    reactive({
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
  })
}

get_citation_column_choices <- function(col_names) {
  ref_cols <- c(
    "databaseReference",
    "compilationReference",
    "originalDataReference"
  )
  doi_cols <- c(
    "databaseDOI",
    "compilationDOI",
    "originalDataDOI"
  )
  bib_cols <- c(
    "databaseBibtex",
    "compilationBibtex",
    "originalDataBibtex"
  )

  list(
    ref_cols = ref_cols[ref_cols %in% col_names],
    doi_cols = doi_cols[doi_cols %in% col_names],
    bib_cols = bib_cols[bib_cols %in% col_names]
  )
}

citationColumnsUI <- function(ns) {
  ns <- NS(ns)
  tagList(
    selectInput(
      ns("reference_cols"),
      "Reference columns (in order):",
      choices = NULL,
      multiple = TRUE
    ),
    selectInput(
      ns("doi_cols"),
      "DOI columns (in order):",
      choices = NULL,
      multiple = TRUE
    ),
    selectInput(
      ns("bibtex_cols"),
      "Bibtex columns (in order):",
      choices = NULL,
      multiple = TRUE
    ),
    # warning goes right below the last input
    uiOutput(ns("cols_warning"))
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

citationColumnsServer <- function(id, column_choices) {
  moduleServer(id, function(input, output, session) {
    # update choices based on citation columns
    observe({
      # select all columns by default if column_choices are different
      if (!identical(column_choices()$ref_cols, column_choices()$doi_cols)) {
        selected_ref <- column_choices()$ref_cols
        selected_doi <- column_choices()$doi_cols
        selected_bib <- column_choices()$bib_cols
      } else {
        selected_ref <- character()
        selected_doi <- character()
        selected_bib <- character()
      }
      updateSelectInput(
        session,
        "reference_cols",
        choices = column_choices()$ref_cols,
        selected = selected_ref
      )
      updateSelectInput(
        session,
        "doi_cols",
        choices = column_choices()$doi_cols,
        selected = selected_doi
      )
      updateSelectInput(
        session,
        "bibtex_cols",
        choices = column_choices()$bib_cols,
        selected = selected_bib
      )
    }) %>% bindEvent(column_choices())

    # --- validation: equal lengths across non-empty inputs ---------------------
    valid_lengths <- reactiveVal(TRUE)
    output$cols_warning <- renderUI({
      ref <- input$reference_cols
      doi <- input$doi_cols
      cit <- input$bibtex_cols

      lens <- c(
        Reference = length(ref %||% character()),
        DOI       = length(doi %||% character()),
        Citation  = length(cit %||% character())
      )
      nonzero <- lens > 0
      if (sum(nonzero) <= 1 || length(unique(lens[nonzero])) == 1) {
        valid_lengths(TRUE)
        return(NULL)
      }

      valid_lengths(FALSE)
      tags$div(
        style = "margin-top: .25rem;",
        tags$small(
          class = "text-danger",
          "Selections must have equal lengths across the non-empty inputs."
        )
      )
    })

    reactive({
      res <- list(
        reference_cols = input$reference_cols,
        doi_cols       = input$doi_cols,
        bibtex_cols    = input$bibtex_cols
      )

      # pass is_valid as attribute
      attr(res, "is_valid") <- valid_lengths()
      return(res)
    })
  })
}
