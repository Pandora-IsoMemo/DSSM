

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
      userSimilaritySelection <- reactiveVal(
        data.frame(
          cols = NA,
          textSimilarity = NA,
          numericSimilarity = NA,
          rounding = NA,
          ignoreEmpty = NA
        )
      )

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
              ignoreEmpty = TRUE
            )
          )
        }
        userSimilaritySelection(userData)

        updateSelectizeInput(
          session = session,
          inputId = "selectedVariable",
          choices = input[["variables"]],
          selected = if (length(input[["variables"]]) == 1) input[["variables"]] else input[["selectedVariable"]]
        )
      }) %>%
        bindEvent(input[["variables"]])

      # change inputs if user made a choice before
      observe({
        req(input[["selectedVariable"]] != "")
        userData <- userSimilaritySelection()
        data <- inputData()

        if (isNumeric(data[, input[["selectedVariable"]]])) {
          shinyjs::hide(id = "textSimilarity")
          shinyjs::show(id = "numericSimilarity")
        } else {
          shinyjs::hide(id = "numericSimilarity")
          shinyjs::show(id = "textSimilarity")
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
      }) %>%
        bindEvent(input[["selectedVariable"]])


      # change user input dataframe based on user selection
      observe({
        req(!is.null(input[["selectedVariable"]]) && input[["selectedVariable"]] != "")
        userData <- userSimilaritySelection()
        userData[userData$cols == input[["selectedVariable"]], "ignoreEmpty"] <- input[["ignoreEmpty"]]
        userData[userData$cols == input[["selectedVariable"]], "textSimilarity"] <- input[["textSimilarity"]]
        userData[userData$cols == input[["selectedVariable"]], "numericSimilarity"] <- input[["numericSimilarity"]]
        userData[userData$cols == input[["selectedVariable"]], "rounding"] <- input[["rounding"]]
        userSimilaritySelection(userData)
      }) %>%
        bindEvent(c(
          input[["textSimilarity"]],
          input[["numericSimilarity"]],
          input[["rounding"]],
          input[["ignoreEmpty"]]
        ))

      # show table when highlight duplicates is clicked
      observe({

        duplicateDataFrames <- findDuplicates(
          data = inputData(),
          userSimilaritySelection = userSimilaritySelection()
        )

        tableData(inputData())

        output$table <- DT::renderDataTable({
          printTab <- DT::datatable(tableData())
          if(length(duplicateDataFrames$allDuplicatesRows)>0){
            printTab %>%
              DT::formatStyle(userSimilaritySelection()$col,
                              backgroundColor = DT::styleRow(duplicateDataFrames$allDuplicatesRows, "pink")
              )
          } else {
            showNotification(paste0("Note: Couldn't find any duplicates for the current selection."))
            printTab
          }

        })
      }) %>%
        bindEvent(input[["highlightDuplicates"]])

      # show table when show duplicates is clicked
      observe({
        duplicateDataFrames <- findDuplicates(
          data = inputData(),
          userSimilaritySelection = userSimilaritySelection()
        )

        tableData(duplicateDataFrames$allDuplicatesDF)

        output$table <- DT::renderDataTable({
          DT::datatable(tableData()) %>%
            DT::formatStyle(userSimilaritySelection()$col,
              backgroundColor = DT::styleRow(1:nrow(tableData()), "pink")
            )
        })
      }) %>%
        bindEvent(input[["showDuplicates"]])

      # show table when show unique rows is clicked
      observe({

        duplicateDataFrames <- findDuplicates(
          data = inputData(),
          userSimilaritySelection = userSimilaritySelection()
        )

        tableData(duplicateDataFrames$uniqueData)

        output$table <- DT::renderDataTable({
          DT::datatable(tableData())
        })
      }) %>%
        bindEvent(input[["showUnique"]])

      observe({
        nRowsRemoved <- nrow(inputData()) - nrow(tableData())
        inputData(tableData())
        output$table <- NULL
        removeModal()
        showNotification(paste0("Removed ",nRowsRemoved," rows from data!"))
      }) %>%
        bindEvent(input[["transferDuplicates"]])

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
