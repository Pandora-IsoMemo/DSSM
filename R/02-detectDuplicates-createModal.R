createModal <- function(vars, session) {
  ns <- session$ns
  modalDialog(
    title = "Duplicate Identifier",
    easyClose = TRUE,
    size = "l",
    footer = modalButton("Close (Without Saving Changes)"),
    tagList(
      HTML("This section gives you the option to find and export duplicates or remove them from the dataset.
                You must first select the columns in which you want to search for duplicates.
                You can then select the degree of similarity, independently for each column, which specifies how similar cases must be to be classified as duplicates.<br><br>
                For numeric columns, you can choose between 'Exact Match' and 'Rounded Match'.
                For the 'Exact Match' selection, observations are only classified as duplicated if the values match exactly.
                For 'Rounded Match', values are classified as identical if they are identical after rounding.
                For this reason, you can specify a rounding factor if you have selected 'Rounded Match'.
                A value of 2, for example, means that the values are rounded to 2 decimal places before being checked for equality.
                A value of 0 means that the values are rounded to whole numbers.
                You can also enter negative numbers, which will result in rounding to the power of ten, e.g. the value -2 rounds to the nearest hundred.<br>
                For text columns you can choose between 'Case Sensitive', where a distinction is made between upper and lower case letters and
                'Case Insensitive', where no distinction is made.<br><br>
                You then have the option to display the duplicates, export them and/or remove them from the dataset."),
      hr(),
      selectizeInput(
        inputId = ns("variables"),
        label = "Duplicate Identifikation Columns",
        choices = vars,
        multiple = TRUE
      ),
      hr(),
      fluidRow(
        column(
          4,
          selectizeInput(
            inputId = ns("selectedVariable"),
            label = "Degree of Similarity for Variable...",
            choices = NULL
          )
        )
      ),
      fluidRow(
        column(
          4,
          shinyjs::hidden(
            selectizeInput(
              inputId = ns("textSimilarity"),
              label = "Text Similarity",
              choices = c(
                "Case Sensitive",
                "Case Insensitive"
              ),
              selected = "Case Sensitive"
            )
          )
        )
      ),
      fluidRow(
        column(
          4,
          shinyjs::hidden(
            selectizeInput(
              inputId = ns("numericSimilarity"),
              label = "Numeric Similarity",
              choices = c(
                "Exact Match",
                "Rounded Match"
              ),
              selected = "Exact Match"
            )
          )
        ),
        column(
          4,
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
      checkboxInput(
        inputId = ns("ignoreEmpty"),
        label = "Ignore Empty Cells",
        value = TRUE
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
          ),
          actionButton(
            inputId = ns("showUnique"),
            label = "Show Unique Rows (Keeps One Row For Each Duplicate)"
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
            inputId = ns("transferDuplicates"),
            label = "Close Modal and Transfer Table to App"
          )
        )
      ),
      hr(),
      DT::dataTableOutput(ns("table"))
    )
  )
}
