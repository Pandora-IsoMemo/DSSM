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
                   preview = NULL,
                   data = list()
                 )

                 mergeList <- reactiveVal(list())

                 ckanFiles <- reactive({
                   getCKANFiles()
                 })

                 dataSource <- reactiveVal(NULL)

                 # select source server ----

                 observeEvent(input$openPopup, ignoreNULL = TRUE, {
                   reset("file")
                   values$warnings <- list()
                   values$errors <- list()
                   values$fileName <- ""
                   values$fileImportSuccess <- NULL
                   values$dataImport <- NULL
                   values$preview <- NULL
                   values$data <- list()
                   dataSource(NULL)

                   showModal(importDataDialog(ns = ns))

                   shinyjs::disable(ns("addData"), asis = TRUE)
                   shinyjs::disable(ns("accept"), asis = TRUE)
                   shinyjs::disable(ns("acceptMerged"), asis = TRUE)
                   shinyjs::disable(ns("acceptQuery"), asis = TRUE)
                   shinyjs::hide(ns("addData"), asis = TRUE)
                   shinyjs::hide(ns("accept"), asis = TRUE)
                   shinyjs::hide(ns("acceptMerged"), asis = TRUE)
                   shinyjs::hide(ns("acceptQuery"), asis = TRUE)

                   titles <-
                     unlist(lapply(ckanFiles(), `[[`, "title"))
                   updateSelectInput(session, "ckanRecord", choices = titles)
                 })

                 observeEvent(input$tabImport, {
                   if (input$tabImport == "Merge") {
                     shinyjs::hide(ns("addData"), asis = TRUE)
                     shinyjs::hide(ns("accept"), asis = TRUE)
                     shinyjs::show(ns("acceptMerged"), asis = TRUE)
                     shinyjs::hide(ns("acceptQuery"), asis = TRUE)
                   } else if (input$tabImport == "Query with SQL") {
                     shinyjs::hide(ns("addData"), asis = TRUE)
                     shinyjs::hide(ns("accept"), asis = TRUE)
                     shinyjs::hide(ns("acceptMerged"), asis = TRUE)
                     shinyjs::show(ns("acceptQuery"), asis = TRUE)
                   } else {
                     shinyjs::show(ns("addData"), asis = TRUE)
                     shinyjs::show(ns("accept"), asis = TRUE)
                     shinyjs::hide(ns("acceptMerged"), asis = TRUE)
                     shinyjs::hide(ns("acceptQuery"), asis = TRUE)
                   }
                 })

                 ckanRecord <- reactive({
                   req(input$ckanRecord)
                   updateSelectInput(session = session, "sheet", selected = character(0))
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

                 observeEvent(input$source, {
                   # reset values
                   values$warnings <- list()
                   values$errors <- list()
                   values$fileName <- ""
                   values$fileImportSuccess <- NULL
                   values$dataImport <- NULL
                   values$preview <- NULL
                   values$data <- list()

                   dataSource(NULL)
                 })

                 observe({
                   req(input$source == "ckan", input$ckanResource)
                   resource <-
                     ckanRecord()$resources[[input$ckanResource]]
                   req(resource)

                   # "file" will be used to load the file
                   # "filename" will be stored in values$fileName
                   dataSource(list(file = resource$url, filename = basename(resource$url)))
                 })

                 observeEvent(input$file, {
                   inFile <- input$file

                   if (is.null(inFile))
                     return()

                   # "file" will be used to load the file
                   # "filename" will be stored in values$fileName
                   dataSource(list(file = inFile$datapath, filename = inFile$name))
                   updateSelectInput(session = session, "sheet", selected = character(0))
                 })

                 observe({
                   req(input$source == "url", input$url)
                   req(trimws(input$url) != "")

                   tmp <- tempfile()

                   res <-
                     try(download.file(input$url, destfile = tmp))
                   if (inherits(res, "try-error")) {
                     alert("Could not load remote file")
                     return()
                   }

                   # "file" will be used to load the file
                   # "filename" will be stored in values$fileName
                   dataSource(list(file = tmp, filename = basename(input$url)))
                   updateSelectInput(session = session, "sheet", selected = character(0))
                 })

                 # specify file server ----

                 observeEvent(list(
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
                   values$warnings <- list()
                   values$errors <- list()
                   values$fileName <- ""
                   values$fileImportSuccess <- NULL
                   values$dataImport <- NULL
                   values$preview <- NULL
                   values$data <- list()

                   withProgress({
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

                     if (length(values$errors) > 0 ||
                         length(values$warnings) > 0) {
                       shinyjs::disable(ns("addData"), asis = TRUE)
                       shinyjs::disable(ns("accept"), asis = TRUE)
                     } else {
                       shinyjs::enable(ns("addData"), asis = TRUE)
                       shinyjs::enable(ns("accept"), asis = TRUE)
                       values$fileImportSuccess <-
                         "Data import successful"
                     }

                     req(values$dataImport)
                     values$preview <- cutAllLongStrings(values$dataImport[1:2, , drop = FALSE], cutAt = 20)
                   },
                   value = 0.75,
                   message = 'loading data ...')
                 })

                 observeEvent(list(input$type, dataSource()$file), {
                   req(input$type)

                   if (input$type %in% c("xls", "xlsx")) {
                     updateSelectInput(session, "sheet",
                                       choices = getSheetSelection(dataSource()$file))
                   }
                 })

                 output$warning <-
                   renderUI(tagList(lapply(values$warnings, tags$p)))
                 output$error <-
                   renderUI(tagList(lapply(values$errors, tags$p)))
                 output$success <-
                   renderText(values$fileImportSuccess)

                 output$preview <- renderDataTable({
                   req(values$preview)
                   DT::datatable(
                     values$preview,
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

                 preparedData <- prepareDataServer(
                   "dataPreparer",
                   selectedData = reactive(values$dataImport),
                   nameOfSelected = reactive(values$fileName)
                   )

                 observeEvent(preparedData(), {
                   req(preparedData())

                   values$dataImport <- preparedData()
                   values$preview <- cutAllLongStrings(values$dataImport[1:2, , drop = FALSE], cutAt = 20)
                   shinyjs::enable(ns("addData"), asis = TRUE)
                 })

                 ## button cancel ----
                 observeEvent(input$cancel, {
                   removeModal()
                 })

                 ## button accept ----
                 observeEvent(input$accept, {
                   removeModal()

                   tmpData <- preparedData()
                   ### format column names for import ----
                   colnames(tmpData) <- colnames(tmpData) %>%
                     formatColumnNames()

                   values$data[[values$fileName]] <- tmpData
                 })

                 ## button add data ----
                 observeEvent(input$addData, {
                   tmpData <- preparedData()
                   ### format column names for import ----
                   colnames(tmpData) <- colnames(tmpData) %>%
                     formatColumnNames()

                   if (values$fileName %in% names(mergeList())) {
                     tmpMergeList <- mergeList()
                     tmpMergeList[[values$fileName]] <- tmpData
                     mergeList(tmpMergeList)
                     showNotification(
                       "File was marked already and was updated successfully."
                     )
                   } else {
                     mergeList(c(
                       mergeList(),
                       setNames(
                         list(tmpData),
                         values$fileName
                       )
                     ))
                   }

                   shinyjs::disable(ns("addData"), asis = TRUE)
                 })

                 ## button merge data ----
                 joinedData <- mergeDataServer("dataMerger", mergeList = mergeList)

                 observeEvent(joinedData(), {
                   if (is.null(joinedData()) ||
                       nrow(joinedData()) == 0) {
                     shinyjs::disable(ns("acceptMerged"), asis = TRUE)
                   } else {
                     shinyjs::enable(ns("acceptMerged"), asis = TRUE)
                   }
                 })

                 observeEvent(input$acceptMerged, {
                   removeModal()

                   values$data[["mergedData"]] <- joinedData()
                 })

                 ## button query data ----
                 queriedData <- mergeViaCommandServer("dataQuerier", mergeList = mergeList)

                 observeEvent(queriedData(), {
                   if (is.null(queriedData()) ||
                       nrow(queriedData()) == 0) {
                     shinyjs::disable(ns("acceptQuery"), asis = TRUE)
                   } else {
                     shinyjs::enable(ns("acceptQuery"), asis = TRUE)
                   }
                 })

                 observeEvent(input$acceptQuery, {
                   removeModal()

                   values$data[["queriedData"]] <- queriedData()
                 })

                 # return value for parent module: ----
                 # currently only the data is returned, not the path(s) to the source(s)
                 reactive(values$data)
               })
}

# import data dialog UI ----
importDataDialog <- function(ns) {
  modalDialog(
    shinyjs::useShinyjs(),
    title = "Import Data",
    style = 'height: 940px',
    footer = tagList(
      actionButton(ns("accept"), "Accept"),
      actionButton(ns("addData"), "Mark for Merge / Query"),
      actionButton(ns("acceptMerged"), "Accept Merged"),
      actionButton(ns("acceptQuery"), "Accept Query"),
      actionButton(ns("cancel"), "Cancel")
    ),
    tabsetPanel(
      id = ns("tabImport"),
      tabPanel("Select (required)",
               selectDataTab(ns = ns)),
      tabPanel("Prepare",
               prepareDataUI(ns("dataPreparer"))),
      tabPanel("Merge",
               mergeDataUI(ns("dataMerger"))),
      tabPanel("Query with SQL",
               mergeViaCommandUI(ns("dataQuerier")))
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
          selectInput(ns("ckanRecord"), "Pandora dataset", choices = NULL, width = "100%"),
          selectizeInput(ns("ckanResource"), "Pandora dataset resource", choices = NULL, width = "100%")
        ),
        conditionalPanel(condition = "input.source == 'file'",
                         ns = ns,
                         fileInput(ns("file"), "File", width = "100%")),
        conditionalPanel(condition = "input.source == 'url'",
                         ns = ns,
                         textInput(ns("url"), "URL", width = "100%"))
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
          selectInput(
            ns("sheet"),
            "Sheet",
            selected = 1,
            choices = 1:10,
            width = "100%"
          )
        )
      )
    ),
    checkboxInput(ns("rownames"), "First column contains rownames"),
    helpText("The first row in your file need to contain variable names."),
    div(
      style = "height: 12.5em",
      div(class = "text-danger", uiOutput(ns("warning"))),
      div(class = "text-danger", uiOutput(ns("error"))),
      div(class = "text-success", textOutput(ns("success")))
    ),
    tags$hr(),
    tags$html(HTML("<b>Preview</b> &nbsp;&nbsp; (Long characters are cutted in the preview)")),
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
  if (is.null(df)) return(NULL)

  df <- lapply(df, function(z) {
    if (!is.character(z))
      return(z)

    cutStrings(charVec = z, cutAt = cutAt)
  }) %>%
    as.data.frame()

  dfColNames <- colnames(df) %>%
    cutStrings(cutAt = max(10, (cutAt - 3)))
  colnames(df) <- dfColNames

  df
}


#' Cut Strings
#'
#' @param charVec (character) character vector
#' @param cutAt (numeric) number of characters after which to cut the entries of an character-column
cutStrings <- function(charVec, cutAt = 50) {
  if (any(nchar(charVec) > cutAt, na.rm = TRUE)) {
    index <- !is.na(charVec) & nchar(charVec) > cutAt
    charVec[index] <- paste0(substr(charVec[index], 1, cutAt), "...")
  }

  charVec
}


#' get nRow
#'
#' @param headOnly (logical) if TRUE, set maximal number of rows to n
#' @param type (character) file type
#' @param n (numeric) maximal number of rows if headOnly
getNrow <- function(headOnly, type, n = 3) {
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


#' Format Column Names
#'
#' Replaces all not alpha-numeric characters in the names of columns with a dot.
#'
#' @param vNames (character) names of the imported data's columns
#' @param isTest (logical) set TRUE if function is used in tests
formatColumnNames <- function(vNames, isTest = FALSE) {
  message <- NULL

  if (any(grepl("[^[:alnum:] | ^\\.]", vNames))) {
    if (!isTest) {
      message <- paste("Warning: One or more column names contain non-alphanumeric characters,",
      "replacing with a dot.")
    }
    # replace non-alphanum characters with dot
    vNames <- gsub("[^[:alnum:] | ^\\.]", ".", vNames)
    # remove dots at the beginning of a column name
    vNames <- gsub("^\\.", "", vNames)
  }

  if (any(grepl("^[0-9]{1,}$", substr(vNames, 1, 1)))) {
    if (!isTest) {
      message <- paste(
        c(message, "Warning: One or more column names begin with a number, adding prefix 'x'."),
        collapse = "\n\n")
    }

    # if name begins with a number paste x before name
    vNames[grepl("^[0-9]{1,}$", substr(vNames, 1, 1))] <-
      paste0("x", vNames[grepl("^[0-9]{1,}$", substr(vNames, 1, 1))])
  }

  if (any(duplicated(vNames))) {
    isDuplicate <- duplicated(vNames)

    if (!isTest) {
      message <- paste(
        c(message,
          paste0("Warning: Duplicated column names found, number added to second occurrence of: \n",
                 paste(vNames[isDuplicate], collapse = ", "))),
        collapse = "\n\n")
    }

    # add number if duplicated names
    inc <- 1
    while(any(isDuplicate)) {
      vNames <- addIncIfDuplicate(vNames, isDuplicate, inc = inc)
      isDuplicate <- duplicated(vNames)
      inc <- inc + 1
    }
  }

  if(!isTest && !is.null(message)) {
    shinyjs::alert(message)
  }

  return(vNames)
}


addIncIfDuplicate <- function(vNames, isDuplicate, inc = 1) {
  vNames[isDuplicate] <- paste0(vNames[isDuplicate], ".", inc)
  vNames
}


#' Get Sheet Selection
#'
#' @param filepath (character) url or path
getSheetSelection <- function(filepath) {
  if (is.null(filepath)) return(list())

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
