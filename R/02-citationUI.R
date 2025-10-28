citationUI <- function(ns) {
  ns <- NS(ns)
  tagList(
    selectInput(
      ns("citationTemplate"),
      "Citation template",
      choices = get_supported_citation_styles()
    ),
    selectInput(
      ns("bib.style"),
      "Citation style",
      choices = c("authoryear")
    ),
    numericInput(
      ns("max.names"),
      "Max. Number of Names",
      min = 1,
      value = 10,
    ),
    checkboxInput(ns("first.inits"), "First names es inits", value = TRUE),
    checkboxInput(ns("dashed"), "Dashed", value = FALSE),
    selectInput(
      ns("sorting"),
      "Sorting",
      choices = c("name-year-title" = "nyt")
    ),
    selectInput(
      ns("no.print.fields"),
      "Omit Fields",
      choices = c("ISSN", "publisher", "month", "doi"),
      multiple = TRUE,
      selected = c("ISSN", "publisher", "month", "doi")
    ),
    selectInput(
      ns("format"),
      "Citation format",
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
        format = input$format
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
        style = input$format
      )
    })

    citation_options
  })
}