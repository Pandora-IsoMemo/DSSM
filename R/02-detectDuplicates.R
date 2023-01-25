

# Module UI ---------------------------------------------------------------

detectDuplicatesUI <- function(id, label = "Detect Duplicates") {
  ns <- NS(id)
  tagList(
    actionButton(ns("showModal"), label = label)
  )
}


# Module Server -----------------------------------------------------------

detectDuplicatesServer <- function(id, inputData) {
  moduleServer(
    id,
    function(input, output, session) {
      # create reactiveVal that will store data later
      tableData <- reactiveVal(NULL)

      # open modal when button is clicked and pass data to modal
      observe({
        showModal(
          createModal(
            vars = names(inputData()),
            session = session
          )
        )
      }) %>%
        bindEvent(input[["showModal"]])

      # show table when highlight duplicates is clicked
      observe({
        check_cols <- input[["variables"]]

        if (input[["numericSimilarity"]] == "Rounded Match") {
          rounding <- input[["rounding"]]
        } else {
          rounding <- NULL
        }

        if (input[["textSimilarity"]] == "Case Sensitive") {
          case_sensitive <- TRUE
        } else {
          case_sensitive <- FALSE
        }

        duplicateDataFrames <- createDuplicateDataFrames(
          inputData = inputData(),
          check_cols = check_cols,
          rounding = rounding,
          case_sensitive = case_sensitive
        )

        tableData(inputData())

        output$table <- renderDataTable({
          datatable(tableData()) %>%
            DT::formatStyle(check_cols,
              backgroundColor = DT::styleRow(duplicateDataFrames$allDuplicatesRows, "pink")
            )
        })
      }) %>%
        bindEvent(input[["highlightDuplicates"]])

      # show table when highlight duplicates is clicked
      observe({
        check_cols <- input[["variables"]]

        if (input[["numericSimilarity"]] == "Rounded Match") {
          rounding <- input[["rounding"]]
        } else {
          rounding <- NULL
        }

        if (input[["textSimilarity"]] == "Case Sensitive") {
          case_sensitive <- TRUE
        } else {
          case_sensitive <- FALSE
        }

        duplicateDataFrames <- createDuplicateDataFrames(
          inputData = inputData(),
          check_cols = check_cols,
          rounding = rounding,
          case_sensitive = case_sensitive
        )

        tableData(duplicateDataFrames$allDuplicatesDF)

        output$table <- renderDataTable({
          datatable(tableData()) %>%
            DT::formatStyle(check_cols,
                            backgroundColor = DT::styleRow(1:nrow(tableData()), "pink")
            )
        })
      }) %>%
        bindEvent(input[["showDuplicates"]])

      # create file for table export
      output$exportDuplicates <- downloadHandler(
        filename = function() {
          "test.csv"
        },
        content = function(file) {
          write.csv(tableData(), file)
        }
      )
    }
  )
}


# Helper Functions --------------------------------------------------------

createModal <- function(vars, session) {
  ns <- session$ns
  modalDialog(
    title = "Duplicate Identifier",
    easyClose = TRUE,
    size = "l",
    footer = modalButton("Back"),
    tagList(
      HTML("This section gives you the option to find and export duplicates or remove them from the dataset.
                You must first select the columns in which you want to search for duplicates.
                Then you can select the degree of similarity in order to be classified as a duplicate.<br><br>
                For numeric columns, you can choose between 'Exact Match' and 'Rounded Match'.
                For the 'Exact Match' selection, observations are only classified as duplicated if the values match exactly.
                For 'Rounded Match', values are classified as identical if they are identical after rounding.
                For this reason, you can specify a rounding factor if you have selected 'Rounded Match'.
                A value of 2, for example, means that the values are rounded to 2 decimal places before being checked for equality.
                A value of 0 means that the values are rounded to whole numbers.
                You can also enter negative numbers, which will result in rounding to the power of ten, e.g. the value -2 rounds to the nearest hundred.<br>
                For text columns you can choose between 'Case Sensitive', where a distinction is made between upper and lower case letters and
                'Case Insensitive', where no distinction is made.<br><br>
                You then have the option to display the duplicates, export them or remove them from the dataset."),
      hr(),
      selectizeInput(
        inputId = ns("variables"),
        label = "Duplicate Identifikation Variables",
        choices = vars,
        multiple = TRUE
      ),
      fluidRow(
        column(
          6,
          selectizeInput(
            inputId = ns("numericSimilarity"),
            label = "Numeric Similarity",
            choices = c(
              "Exact Match",
              "Rounded Match"
            ),
            selected = "Exact Match"
          )
        ),
        column(
          6,
          conditionalPanel(
            condition = "input.numericSimilarity == 'Rounded Match'",
            ns = ns,
            numericInput(
              inputId = ns("rounding"),
              label = "Rounding Factor",
              value = 0,
              min = -10,
              max = 10
            )
          )
        )
      ),
      selectizeInput(
        inputId = ns("textSimilarity"),
        label = "Text Similarity",
        choices = c(
          "Case Sensitive",
          "Case Insensitive"
        ),
        selected = "Case Sensitive"
      ),
      hr(),
      fluidRow(
        column(
          12,
          actionButton(
            inputId = ns("highlightDuplicates"),
            label = "Highlight Duplicated Rows"
          ),
          actionButton(
            inputId = ns("showDuplicates"),
            label = "Show Duplicated Rows Only"
          )
        )
      ),
      br(),
      fluidRow(
        column(
          12,
          downloadButton(
            outputId = ns("exportDuplicates"),
            label = "Export Table"
          ),
          actionButton(
            inputId = ns("removeDuplicates"),
            label = "Close Modal and Transfer Table to App"
          )
        )
      ),
      hr(),
      DT::dataTableOutput(ns("table"))
    )
  )
}

createDuplicateDataFrames <- function(inputData, check_cols, rounding = NULL, case_sensitive) {
  cols <- check_cols
  numericCols <- numericColumns(inputData)[numericColumns(inputData) %in% cols]
  characterCols <- characterColumns(inputData)[characterColumns(inputData) %in% cols]

  preparedData <- inputData

  if (!is.null(rounding)) {
    preparedData[numericCols] <- sapply(preparedData[numericCols], round, rounding)
  }
  if (case_sensitive == FALSE) {
    preparedData[characterCols] <- sapply(preparedData[characterCols], tolower)
  }

  allDuplicatesDF <- inputData[duplicated(preparedData[, cols]) | duplicated(preparedData[, cols], fromLast = TRUE), ]
  uniqueData <- inputData[!duplicated(preparedData[, cols]), ]

  list(
    allDuplicatesDF = allDuplicatesDF,
    allDuplicatesRows = rownames(allDuplicatesDF),
    uniqueData = uniqueData
  )
}

# TEST MODULE -------------------------------------------------------------

ui <- fluidPage(
  detectDuplicatesUI(id = "detectDuplicates")
)

server <- function(input, output, session) {
  isoDataFull <- reactiveVal(NULL)

  testdata <- read.csv2("inst/extdata/duplicates_example.csv")
  isoDataFull(testdata)

  detectDuplicatesServer(
    id = "detectDuplicates",
    inputData = isoDataFull
  )
}

shinyApp(ui, server)
