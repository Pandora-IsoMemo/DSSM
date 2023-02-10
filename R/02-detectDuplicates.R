

# Module UI ---------------------------------------------------------------

#' detectDuplicates module ui
#'
#' @param id module id
#' @param label label for actionButton which will open a modal
#' @export
detectDuplicatesUI <- function(id, label = "Detect Duplicates") {
  ns <- NS(id)
  tagList(
    actionButton(ns("showModal"), label = label)
  )
}


# Module Server -----------------------------------------------------------

#' detectDuplicates module server
#'
#' @param id module id
#' @param inputData dataframe in which duplicates are searched for
#' @export
detectDuplicatesServer <- function(id, inputData) {
  moduleServer(
    id,
    function(input, output, session) {
      # create reactiveVal that will store data later
      tableData <- reactiveVal(NULL)
      userSimilaritySelection <- reactiveVal(
        data.frame(
          cols = NA,
          textSimilarity = NA,
          numericSimilarity = NA,
          rounding = NA,
          ignoreEmpty = NA,
          ignoreSpaces = NA
        )
      )

      # open modal when button is clicked and pass data to modal
      observe({
        showModal(
          createDuplicateModal(
            vars = names(inputData()),
            session = session
          )
        )
      }) %>%
        bindEvent(input[["showModal"]])

      # create dataframe with user inputs and update choices for selectedVariable
      observe({
        userData <- userSimilaritySelection()
        userData <- userData[userData$cols %in% input[["variables"]], ]
        for (i in input[["variables"]][!input[["variables"]] %in% userData$cols]) {
          userData <- rbind(
            userData,
            data.frame(
              cols = i,
              textSimilarity = "Case Sensitive",
              numericSimilarity = "Exact Match",
              rounding = 0,
              ignoreEmpty = TRUE,
              ignoreSpaces = FALSE
            )
          )
        }
        userSimilaritySelection(userData)

        if (length(input[["variables"]]) == 1) {
          selected <- input[["variables"]]
          choices <- input[["variables"]]
        } else if (length(input[["variables"]]) == 0) {
          selected <- ""
          choices <- ""
        } else {
          selected <- input[["selectedVariable"]]
          choices <- input[["variables"]]
        }

        updateSelectizeInput(
          session = session,
          inputId = "selectedVariable",
          choices = choices,
          selected = selected
        )
      }) %>%
        bindEvent(input[["variables"]],
          ignoreNULL = FALSE,
          ignoreInit = TRUE
        )

      # change inputs if user made a choice before
      observe({
        req(input[["selectedVariable"]] != "")
        userData <- userSimilaritySelection()
        data <- inputData()

        if (isNumeric(data[, input[["selectedVariable"]]])) {
          shinyjs::hide(id = "textSimilarity")
          shinyjs::hide(id = "ignoreSpaces")
          shinyjs::show(id = "numericSimilarity")
        } else {
          shinyjs::hide(id = "numericSimilarity")
          shinyjs::show(id = "textSimilarity")
          shinyjs::show(id = "ignoreSpaces")
        }

        updateSelectizeInput(
          session = session,
          inputId = "textSimilarity",
          selected = userData[userData$cols == input[["selectedVariable"]], "textSimilarity"]
        )
        updateSelectizeInput(
          session = session,
          inputId = "numericSimilarity",
          selected = userData[userData$cols == input[["selectedVariable"]], "numericSimilarity"]
        )
        updateNumericInput(
          session = session,
          inputId = "rounding",
          value = userData[userData$cols == input[["selectedVariable"]], "rounding"]
        )
        updateCheckboxInput(
          session = session,
          inputId = "ignoreEmpty",
          value = userData[userData$cols == input[["selectedVariable"]], "ignoreEmpty"]
        )
        updateCheckboxInput(
          session = session,
          inputId = "ignoreSpaces",
          value = userData[userData$cols == input[["selectedVariable"]], "ignoreSpaces"]
        )
      }) %>%
        bindEvent(input[["selectedVariable"]])


      # change user input dataframe based on user selection
      observe({
        req(!is.null(input[["selectedVariable"]]) && input[["selectedVariable"]] != "")
        userData <- userSimilaritySelection()
        userData[userData$cols == input[["selectedVariable"]], "ignoreEmpty"] <- input[["ignoreEmpty"]]
        userData[userData$cols == input[["selectedVariable"]], "ignoreSpaces"] <- input[["ignoreSpaces"]]
        userData[userData$cols == input[["selectedVariable"]], "textSimilarity"] <- input[["textSimilarity"]]
        userData[userData$cols == input[["selectedVariable"]], "numericSimilarity"] <- input[["numericSimilarity"]]
        userData[userData$cols == input[["selectedVariable"]], "rounding"] <- input[["rounding"]]
        userSimilaritySelection(userData)
      }) %>%
        bindEvent(c(
          input[["textSimilarity"]],
          input[["numericSimilarity"]],
          input[["rounding"]],
          input[["ignoreEmpty"]],
          input[["ignoreSpaces"]]
        ))

      # show table when highlight duplicates is clicked
      observe({
        duplicateDataFrames <- findDuplicates(
          data = inputData(),
          userSimilaritySelection = userSimilaritySelection(),
          addColumn = input[["addColumn"]]
        )

        tableData(duplicateDataFrames$inputData)

        printData <- cutAllLongStrings(tableData(), cutAt = 20)
        rownames(printData) <- rownames(tableData())

        output$table <- DT::renderDataTable({
          printTab <- DT::datatable(printData,
            options = list(
              scrollX = TRUE
            )
          )

          # Coloring Rows is currently disabled because an error appears for large data (Maximum call stack size exceeded)
          # if(length(duplicateDataFrames$allDuplicatesRows)>0){
          #   printTab %>%
          #     DT::formatStyle(userSimilaritySelection()$col,
          #                     backgroundColor = DT::styleRow(
          #                       duplicateDataFrames$allDuplicatesRows,
          #                       "pink")
          #     )
          # } else {
          #   showNotification(paste0("Note: Couldn't find any duplicates for the current selection."))
          #   printTab
          # }
        })
      }) %>%
        bindEvent(input[["highlightDuplicates"]])

      # show table when show duplicates is clicked
      observe({
        duplicateDataFrames <- findDuplicates(
          data = inputData(),
          userSimilaritySelection = userSimilaritySelection(),
          addColumn = input[["addColumn"]]
        )

        tableData(duplicateDataFrames$allDuplicatesDF)

        printData <- cutAllLongStrings(tableData(), cutAt = 20)
        rownames(printData) <- rownames(tableData())

        output$table <- DT::renderDataTable({
          DT::datatable(printData,
            options = list(
              scrollX = TRUE
            )
          )
        })
      }) %>%
        bindEvent(input[["showDuplicates"]])

      # show table when show unique rows is clicked
      observe({
        duplicateDataFrames <- findDuplicates(
          data = inputData(),
          userSimilaritySelection = userSimilaritySelection(),
          addColumn = input[["addColumn"]]
        )

        tableData(duplicateDataFrames$uniqueData)

        printData <- cutAllLongStrings(tableData(), cutAt = 20)
        rownames(printData) <- rownames(tableData())

        output$table <- DT::renderDataTable({
          DT::datatable(printData,
            options = list(
              scrollX = TRUE
            )
          )
        })
      }) %>%
        bindEvent(input[["showUnique"]])

      observe({
        nRowsRemoved <- nrow(inputData()) - nrow(tableData())
        inputData(tableData())
        output$table <- NULL
        removeModal()
        showNotification(paste0("Removed ", nRowsRemoved, " rows from dataset!"))
      }) %>%
        bindEvent(input[["transferDuplicates"]])

      # create file for table export
      # output$exportDuplicates <- downloadHandler(
      #   filename = function() {
      #     "test.csv"
      #   },
      #   content = function(file) {
      #     write.csv(tableData(), file)
      #   }
      # )
    }
  )
}



# TEST MODULE -------------------------------------------------------------

ui <- fluidPage(
  shinyjs::useShinyjs(),
  detectDuplicatesUI(id = "detectDuplicates"),
  DT::dataTableOutput("tab")
)

server <- function(input, output, session) {
  isoDataFull <- reactiveVal(NULL)

  testdata <- read.csv2("inst/extdata/duplicates_example.csv")
  isoDataFull(testdata)

  detectDuplicatesServer(
    id = "detectDuplicates",
    inputData = isoDataFull
  )

  output$tab <- DT::renderDataTable({
    isoDataFull()
  })
}

shinyApp(ui, server)
