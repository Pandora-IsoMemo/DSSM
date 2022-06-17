#' Data import module
#'
#' Displays a button which opens a import dialog when clicked
#'
#' @param id id of module
#' @param label label of button
#' @rdname importData
#'
importDataUI <- function(id, label = "Import Data") {
  ns <- NS(id)
  actionButton(ns("openPopup"), label)
}

#' Server function for data import
#'
#' Backend for data import module
#'
#' @param id namespace id
#' @param rowNames (reactive) use this for rownames of imported data
#' @param colNames (reactive) use this for colnames of imported data
#' @param customWarningChecks list of reactive functions which will be executed after importing of data.
#'   functions need to return TRUE if check is successful or a character with a warning otherwise.
#' @param customErrorChecks list of reactive functions which will be executed after importing of data.
#'   functions need to return TRUE if check is successful or a character with a warning otherwise.
#'
importDataServer <- function(id,
                             rowNames = NULL,
                             colNames = NULL,
                             customWarningChecks = list(),
                             customErrorChecks = list()) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns

                 values <- reactiveValues(
                   warnings = list(),
                   errors = list(),
                   fileName = NULL,
                   fileImportSuccess = NULL,
                   dataImport = NULL,
                   data = list()
                 )

                 ckanFiles <- reactive({
                   getCKANFiles()
                 })

                 dataSource <- reactiveVal(NULL)

                 # select source server ----
                 observeEvent(input$openPopup, ignoreNULL = TRUE, {
                   reset("file")
                   values$warnings <- list()
                   values$errors <- list()
                   values$fileImportWarning <- NULL
                   values$fileImportSuccess <- NULL
                   values$dataImport <- NULL
                   values$data <- list()
                   dataSource(NULL)

                   showModal(importDataDialog(ns = ns))

                   titles <- unlist(lapply(ckanFiles(), `[[`, "title"))
                   updateSelectInput(session, "ckanRecord", choices = titles)
                 })

                 ckanRecord <- reactive({
                   req(input$ckanRecord)
                   ckanFiles()[[input$ckanRecord]]
                 })

                 ckanResources <- reactive({
                   req(ckanRecord())

                   resources <- names(ckanRecord()$resources)
                   labels <- unlist(lapply(ckanRecord()$resources, function(x) {
                     paste(x$name, " (", x$format, ")")
                   }))
                   setNames(resources, labels)
                 })

                 observeEvent(ckanResources(), {
                   choices <- ckanResources()
                   updateSelectizeInput(session, "ckanResource", choices = choices)
                 })

                 observe({
                   req(input$source == "ckan")
                   resource <- ckanRecord()$resources[[input$ckanResource]]
                   req(resource)
                   dataSource(list(file = resource$url, filename = resource$url))
                 })

                 observeEvent(input$file, {
                   inFile <- input$file
                   filename <- inFile$name

                   if (is.null(inFile))
                     return()

                   dataSource(list(file = inFile$datapath, filename = filename))
                 })

                 observeEvent(input$url, {
                   req(input$url)
                   req(trimws(input$url) != "")

                   tmp <- tempfile()

                   res <- try(download.file(input$url, destfile = tmp))
                   if (inherits(res, "try-error")) {
                     alert("Could not load remote file")
                     return()
                   }

                   dataSource(list(file = tmp, filename = input$url))
                 })

                 # specify file server ----
                 observeEvent(list(
                   dataSource(),
                   input$type,
                   input$colSep,
                   input$decSep,
                   input$rownames#,
                   #input$includeSd
                 ),
                 {
                   req(dataSource())

                   # reset values
                   values$dataImport <- NULL
                   values$warnings <- list()
                   values$errors <- list()
                   values$fileImportSuccess <- NULL

                   values <- loadDataWrapper(
                     values = values,
                     filepath = dataSource()$file,
                     filename = dataSource()$filename,
                     colNames = colNames,
                     type = input$type,
                     sep = input$colSep,
                     dec = input$decSep,
                     withRownames = isTRUE(input$rownames),
                     headOnly = FALSE,
                     customWarningChecks = customWarningChecks,
                     customErrorChecks = customErrorChecks)

                   if (length(values$errors) > 0) {
                     shinyjs::disable("accept")
                     return(NULL)
                   }

                   shinyjs::enable("accept")
                   values$fileImportSuccess <- "Data import was successful"
                 })

                 output$warning <-
                   renderUI(tagList(lapply(values$warnings, tags$p)))
                 output$error <- renderUI(tagList(lapply(values$errors, tags$p)))
                 output$success <- renderText(values$fileImportSuccess)

                 output$preview <- renderTable(
                   values$headData,
                   bordered = TRUE,
                   rownames = FALSE,
                   colnames = TRUE
                 )

                 observeEvent(input$cancel, {
                   removeModal()
                 })

                 observeEvent(input$accept, {
                   removeModal()

                   values$data[[values$fileName]] <- values$dataImport
                 })

                 reactive(values$data)
               })
}

# import data dialog ui
importDataDialog <- function(ns){
  modalDialog(
    useShinyjs(),
    title = "Import Data",
    footer = tagList(
      actionButton(ns("cancel"), "Cancel"),
      actionButton(ns("addData"), "Add data"),
      actionButton(ns("accept"), "Accept")
    ),
    tabsetPanel(
      tabPanel(
        "Select Data",
        selectDataUI(ns = ns)
      ),
      tabPanel(
        "Merge Data",
        mergeDataUI(ns("dataMerger"))
      )
    )
  )
}

#' Select Data UI
#'
#' @param ns namespace
selectDataUI <- function(ns){
  tagList(
    # select source UI ----
    selectInput(ns("source"), "Source", choices = c("Pandora Platform" = "ckan","File" = "file", "URL" = "url")),
    conditionalPanel(
      condition = "input.source == 'ckan'",
      ns = ns,
      selectInput(ns("ckanRecord"), "Pandora dataset", choices = NULL),
      selectizeInput(ns("ckanResource"), "Pandora dataset resource", choices = NULL
      )
    ),
    conditionalPanel(
      condition = "input.source == 'file'",
      ns = ns,
      fileInput(ns("file"), "File")
    ),
    conditionalPanel(
      condition = "input.source == 'url'",
      ns = ns,
      textInput(ns("url"), "URL")
    ),
    tags$hr(),
    # specify file UI ----
    selectInput(
      ns("type"),
      "File type",
      choices = c("xls(x)" = "xlsx", "csv", "ods", "txt"),
      selected = "xlsx"
    ),
    conditionalPanel(
      condition = paste0("input.type == 'csv' || input.type == 'txt'"),
      div(style = "display: inline-block;horizontal-align:top; width: 80px;",
          textInput(ns("colSep"), "column separator:", value = ",")),
      div(style = "display: inline-block;horizontal-align:top; width: 80px;",
          textInput(ns("decSep"), "decimal separator:", value = ".")),
      ns = ns
    ),
    checkboxInput(ns("rownames"), "First column contains rownames"),
    helpText(
      "The first row in your file need to contain variable names."
    ),
    div(class = "text-danger", uiOutput(ns("warning"))),
    div(class = "text-danger", uiOutput(ns("error"))),
    div(class = "text-success", textOutput(ns("success"))),
    tableOutput(ns("preview"))
  )
}

#' Load Data Wrapper
#'
#' @inheritParams importDataServer
#' @param values (list) list with import specifications
#' @param filepath url or path
#' @param filename url or file name
#' @param type (character) file type input
#' @param sep (character) column separator input
#' @param dec (character) decimal separator input
#' @param withRownames (logical) contains rownames input
#' @param headOnly (logical) load only head (first n rows) of file
loadDataWrapper <- function(values, filepath, filename, colNames,
                            type, sep, dec, withRownames, headOnly,
                            customWarningChecks, customErrorChecks) {
  df <- tryCatch(
    loadData(
      file = filepath,
      type = type,
      sep = sep,
      dec = dec,
      rownames = withRownames,
      headOnly = headOnly
    ),
    error = function(e) {
      values$warnings <- c(values$warnings, "Could not read in file.")
      shinyjs::disable("accept")
      NULL
    },
    warning = function(w) {
      values$warnings <- c(values$warnings, "Could not read in file.")
      shinyjs::disable("accept")
      NULL
    }
  )

  if (is.null(df)) {
    values$headData <- NULL
    return(NULL)
  }

  #attr(df, "includeSd") <- isTRUE(input$includeSd)

  ## set colnames
  if (!is.null(colNames)) {
    colnames(df) <- rep("", ncol(df))
    mini <- min(length(colNames()), ncol(df))
    colnames(df)[seq_len(mini)] <- colNames()[seq_len(mini)]
  }

  ## Import technically successful
  values$fileName <- filename
  values$dataImport <- as.data.frame(df)

  ## create preview data
  values$headData <- lapply(head(as.data.frame(df)), function(z) {
    if (is.character(z)) {
      substr(z, 1, 50)
    } else {
      z
    }
  })[1:min(ncol(df), 5)]

  ## Import valid?
  lapply(customWarningChecks, function(fun) {
    res <- fun()(df)
    if (!isTRUE(res)) {
      values$warnings <- c(values$warnings, res)
    }
  })

  lapply(customErrorChecks, function(fun) {
    res <- fun()(df)
    if (!isTRUE(res)) {
      values$errors <- c(values$errors, res)
    }
  })

  values
}


loadData <-
  function(file,
           type,
           sep = ",",
           dec = ".",
           rownames = FALSE,
           headOnly = FALSE) {
    # if(type == "csv" | type == "txt"){
    #   codepages <- setNames(iconvlist(), iconvlist())
    #   x <- lapply(codepages, function(enc) try(suppressWarnings({read.csv(file,
    #                                                     fileEncoding=enc,
    #                                                     sep = sep, dec = dec,
    #                                                     stringsAsFactors = FALSE,
    #                                                     row.names = NULL,
    #                                                     nrows=3, header=TRUE)}),
    #                                            silent = TRUE)) # you get lots of errors/warning here
    #   x <- x[!sapply(x, function(y) class(y) %in% "try-error")]
    #   maybe_ok <- which(sapply(x, function(y) isTRUE(all.equal(dim(y)[1], c(3)))))
    #   if(length(maybe_ok) > 0){
    #     encTry <- names(maybe_ok[1])
    #   } else {
    #     encTry <- ""
    #   }
    # }
    encTry <- as.character(guess_encoding(file)[1, 1])
    if (type == "xlsx") {
      xlsSplit <- strsplit(file, split = "\\.")[[1]]
      if (xlsSplit[length(xlsSplit)] == "xls") {
        type <- "xls"
      }
    }
    data <- switch(
      type,
      csv = suppressWarnings({
        read.csv(
          file,
          sep = sep,
          dec = dec,
          stringsAsFactors = FALSE,
          row.names = NULL,
          fileEncoding = encTry,
          nrows = getNrow(headOnly, type)
        )
      }),
      txt = suppressWarnings({
        read.csv(
          file,
          sep = sep,
          dec = dec,
          stringsAsFactors = FALSE,
          row.names = NULL,
          fileEncoding = encTry,
          nrows = getNrow(headOnly, type)
        )
      }),
      xlsx = read.xlsx(file, rows = getNrow(headOnly, type)),
      xls = suppressWarnings({
        readxl::read_excel(file, n_max = getNrow(headOnly, type))
      }),
      ods = readODS::read_ods(file, range = getNrow(headOnly, type))
    )

    if (is.null(data))
      return(NULL)

    if (is.null(dim(data))) {
      stop("Could not determine dimensions of data")
      return(NULL)
    }

    if (any(dim(data) == 1)) {
      warning("Number of rows or columns equal to 1")
      return(NULL)
    }

    if (any(dim(data) == 0)) {
      stop("Number of rows or columns equal to 0")
      return(NULL)
    }

    if (rownames) {
      rn <- data[, 1]
      data <- data[, -1, drop = FALSE]
      rownames(data) <- rn
    }
    data <- convertNumeric(data)

    return(data)
  }

#' get nRow
#'
#' @param headOnly (logical) if TRUE, set maximal number of rows to n
#' @param type (character) file type
#' @param n (numeric) maximal number of rows if headOnly
getNrow <- function(headOnly, type, n = 3) {
  if (headOnly) {
    if (type == "xlsx") return(1:n) else
      if (type == "ods") return(paste0("A1:C", n)) else
        return(n)
  } else {
    if (type %in% c("xlsx", "ods")) return(NULL) else
      if (type == "xls") return(Inf) else
        return(-999)
  }
}
