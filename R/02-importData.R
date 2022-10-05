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
#' @param customWarningChecks list of reactive functions which will be executed after importing
#'  of data.
#'   functions need to return TRUE if check is successful or a character with a warning otherwise.
#' @param customErrorChecks list of reactive functions which will be executed after importing
#' of data.
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

                   titles <-
                     unlist(lapply(ckanFiles(), `[[`, "title"))
                   updateSelectInput(session, "ckanRecord", choices = titles)
                 })

                 ckanRecord <- reactive({
                   req(input$ckanRecord)
                   ckanFiles()[[input$ckanRecord]]
                 })

                 ckanResources <- reactive({
                   req(ckanRecord())

                   resources <- names(ckanRecord()$resources)
                   labels <-
                     unlist(lapply(ckanRecord()$resources, function(x) {
                       paste(x$name, " (", x$format, ")")
                     }))
                   setNames(resources, labels)
                 })

                 observeEvent(ckanResources(), {
                   choices <- ckanResources()
                   updateSelectizeInput(session, "ckanResource", choices = choices)
                 })

                 observe({
                   req(input$source == "ckan", input$ckanResource)
                   resource <-
                     ckanRecord()$resources[[input$ckanResource]]
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

                   res <-
                     try(download.file(input$url, destfile = tmp))
                   if (inherits(res, "try-error")) {
                     alert("Could not load remote file")
                     return()
                   }

                   dataSource(list(file = tmp, filename = input$url))
                 })

                 observeEvent(list(input$type, dataSource()$file), {
                   req(dataSource()$file)

                   if (input$type %in% c("xls", "xlsx")) {
                     updateSelectInput(session, "sheet",
                                       choices = getSheetSelection(dataSource()$file))
                   }
                 })

                 # specify file server ----
                 observeEvent(
                   list(
                     dataSource(),
                     input$type,
                     input$colSep,
                     input$decSep,
                     input$rownames,
                     #input$includeSd
                     input$sheet
                   ),
                   {
                     req(dataSource())

                     # reset values
                     values$dataImport <- NULL
                     values$warnings <- list()
                     values$errors <- list()
                     values$fileImportSuccess <- NULL

                     withProgress({
                       # load first lines only
                       values <- loadDataWrapper(
                         values = values,
                         filepath = dataSource()$file,
                         filename = dataSource()$filename,
                         colNames = colNames,
                         type = input$type,
                         sep = input$colSep,
                         dec = input$decSep,
                         withRownames = isTRUE(input$rownames),
                         sheetId = as.numeric(input$sheet),
                         headOnly = TRUE,
                         customWarningChecks = customWarningChecks,
                         customErrorChecks = customErrorChecks
                       )
                     },
                     value = 0.75,
                     message = 'load preview data ...')

                     if (length(values$errors) > 0 ||
                         length(values$warnings) > 0) {
                       shinyjs::disable(ns("accept"), asis = TRUE)
                     } else {
                       shinyjs::enable(ns("accept"), asis = TRUE)
                       values$fileImportSuccess <-
                         "Data import was successful"
                     }
                   }
                 )

                 output$warning <-
                   renderUI(tagList(lapply(values$warnings, tags$p)))
                 output$error <-
                   renderUI(tagList(lapply(values$errors, tags$p)))
                 output$success <-
                   renderText(values$fileImportSuccess)

                 output$preview <- renderDataTable({
                   req(values$dataImport)

                   previewData <-
                     cutAllLongStrings(values$dataImport, cutAt = 20)
                   DT::datatable(
                     previewData,
                     filter = "none",
                     selection = "none",
                     rownames = FALSE,
                     options = list(
                       dom = "t",
                       ordering = FALSE,
                       scrollX = TRUE
                     )
                   )
                 })

                 observeEvent(input$cancel, {
                   removeModal()
                 })

                 observeEvent(input$accept, {
                   removeModal()

                   withProgress({
                     # load full data set
                     values <- loadDataWrapper(
                       values = values,
                       filepath = dataSource()$file,
                       filename = dataSource()$filename,
                       colNames = colNames,
                       type = input$type,
                       sep = input$colSep,
                       dec = input$decSep,
                       withRownames = isTRUE(input$rownames),
                       sheetId = as.numeric(input$sheet),
                       headOnly = FALSE,
                       customWarningChecks = customWarningChecks,
                       customErrorChecks = customErrorChecks
                     )
                   },
                   value = 0.75,
                   message = 'import full data ...')

                   values$data[[values$fileName]] <-
                     values$dataImport
                 })

                 reactive(values$data)
               })
}

# import data dialog ui
importDataDialog <- function(ns) {
  modalDialog(
    shinyjs::useShinyjs(),
    title = "Import Data",
    footer = tagList(#actionButton(ns("addData"), "Add data"),
      actionButton(ns("accept"), "Accept"),
      actionButton(ns("cancel"), "Cancel")),
    tabsetPanel(tabPanel("Select Data",
                         selectDataTab(ns = ns))#,
                # tabPanel("Merge Data",
                #          mergeDataUI(ns("dataMerger")))
                )
    )
}

#' Select Data UI
#'
#' @param ns namespace
selectDataTab <- function(ns) {
  tagList(
    tags$br(),
    fluidRow(
      column(4,
             # select source UI ----
             selectInput(
               ns("source"),
               "Source",
               choices = c(
                 "Pandora Platform" = "ckan",
                 "File" = "file",
                 "URL" = "url"
               )
             )),
      column(
        8,
        conditionalPanel(
          condition = "input.source == 'ckan'",
          ns = ns,
          selectInput(ns("ckanRecord"), "Pandora dataset", choices = NULL),
          selectizeInput(ns("ckanResource"), "Pandora dataset resource", choices = NULL)
        ),
        conditionalPanel(condition = "input.source == 'file'",
                         ns = ns,
                         fileInput(ns("file"), "File")),
        conditionalPanel(condition = "input.source == 'url'",
                         ns = ns,
                         textInput(ns("url"), "URL"))
      )
    ),
    tags$hr(),
    # specify file UI ----
    fluidRow(
      column(4,
             selectInput(
               ns("type"),
               "File type",
               choices = c("xls(x)" = "xlsx", "csv", "ods", "txt"),
               selected = "xlsx"
             )),
      column(
        8,
        conditionalPanel(
          condition = paste0("input.type == 'csv' || input.type == 'txt'"),
          ns = ns,
          fluidRow(column(
            width = 5,
            textInput(ns("colSep"), "column separator:", value = ",")
          ),
          column(
            width = 5,
            textInput(ns("decSep"), "decimal separator:", value = ".")
          ))
        ),
        conditionalPanel(
          condition = paste0("input.type == 'xlsx' || input.type == 'xlsx'"),
          ns = ns,
          fluidRow(column(
            width = 10,
            selectInput(
              ns("sheet"),
              "Sheet",
              selected = 1,
              choices = 1:10,
              width = "100%"
            )
          ))
        )
      )
    ),
    checkboxInput(ns("rownames"), "First column contains rownames"),
    helpText("The first row in your file need to contain variable names."),
    div(class = "text-danger", uiOutput(ns("warning"))),
    div(class = "text-danger", uiOutput(ns("error"))),
    div(class = "text-success", textOutput(ns("success"))),
    tags$br(),
    tags$h5("Preview:"),
    fluidRow(column(12,
                    dataTableOutput(ns(
                      "preview"
                    ))))
  )
}

#' Load Data Wrapper
#'
#' @inheritParams importDataServer
#' @param values (list) list with import specifications
#' @param filepath (character) url or path
#' @param filename (character) url or file name
#' @param type (character) file type input
#' @param sep (character) column separator input
#' @param dec (character) decimal separator input
#' @param withRownames (logical) contains rownames input
#' @param sheetId (numeric) sheet id
#' @param headOnly (logical) load only head (first n rows) of file
loadDataWrapper <- function(values,
                            filepath,
                            filename,
                            colNames,
                            type,
                            sep,
                            dec,
                            withRownames,
                            sheetId,
                            headOnly,
                            customWarningChecks,
                            customErrorChecks) {
  df <- tryCatch(
    loadData(
      file = filepath,
      type = type,
      sep = sep,
      dec = dec,
      rownames = withRownames,
      sheetId = sheetId,
      headOnly = headOnly
    ),
    error = function(e) {
      values$errors <- c(values$errors, "Could not read in file.")
      NULL
    },
    warning = function(w) {
      values$warnings <- c(values$warnings, "Could not read in file.")
      NULL
    }
  )

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
           sheetId = 1,
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
      xlsx = read.xlsx(file, sheet = sheetId, rows = getNrow(headOnly, type)),
      xls = suppressWarnings({
        readxl::read_excel(file, sheet = sheetId, n_max = getNrow(headOnly, type))
      }),
      ods = readODS::read_ods(file, sheet = sheetId, range = getNrow(headOnly, type))
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


#' Cut All Strings
#'
#' @param df (data.frame) data.frame with character and non-character columns
#' @param cutAt (numeric) number of characters after which to cut the entries of an character-column
cutAllLongStrings <- function(df, cutAt = 50) {
  cutStrings <- function(vec, cutAt) {
    if (any(nchar(vec) > cutAt, na.rm = TRUE)) {
      index <- !is.na(vec) & nchar(vec) > cutAt
      vec[index] <- paste0(substr(vec[index], 1, cutAt), "...")
    }

    vec
  }

  df <- lapply(df, function(z) {
    if (!is.character(z))
      return(z)

    cutStrings(z, cutAt = cutAt)
  }) %>%
    as.data.frame()

  dfColNames <- colnames(df) %>%
    cutStrings(cutAt = max(10, (cutAt - 3)))
  colnames(df) <- dfColNames

  df
}


#' get nRow
#'
#' @param headOnly (logical) if TRUE, set maximal number of rows to n
#' @param type (character) file type
#' @param n (numeric) maximal number of rows if headOnly
getNrow <- function(headOnly, type, n = 4) {
  if (headOnly) {
    if (type == "xlsx")
      return(1:n)
    else
      if (type == "ods")
        return(paste0("A1:C", n))
    else
      return(n)
  } else {
    if (type %in% c("xlsx", "ods"))
      return(NULL)
    else
      if (type == "xls")
        return(Inf)
    else
      return(-999)
  }
}


#' Get Sheet Selection
#'
#' @param filepath (character) url or path
getSheetSelection <- function(filepath) {
  fileSplit <- strsplit(filepath, split = "\\.")[[1]]
  typeOfFile <- fileSplit[length(fileSplit)]

  if (!(typeOfFile %in% c("xls", "xlsx")))
    return(NULL)

  if (typeOfFile == "xlsx") {
    # loadWorkbook() is also able to handle url's
    sheetNames <- loadWorkbook(filepath) %>% names()
  } else if (typeOfFile == "xls") {
    sheetNames <- excel_sheets(filepath)
  }

  if (length(sheetNames) == 0)
    return(NULL)

  sheets <- 1:length(sheetNames)
  names(sheets) <- sheetNames

  sheets
}
