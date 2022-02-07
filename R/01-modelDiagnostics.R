modelDiagButton <- function(id, title = "Show Model Diagnostics") {
  ns <- NS(id)
  actionButton(ns("showDiag"), title)
}

modelDiagnostics <- function(input, output, session, model, choice = FALSE) {
  if(choice){
  observeEvent(input$showDiag, {
    showModal(modalDialog(
      "Model Diagnostics",
      easyClose = TRUE,
      footer = modalButton("OK"),
      selectInput(session$ns("modelChoice"), label = "Select model", choices = names(model()$models)),
      radioButtons(session$ns("diagType"), label = "Diagnostics Type",
                   choices = c("Gelman Scale Reduction Factor" = "gelman",
                               "Raftery and Lewis" = "raftery",
                               "Geweke z-Score" = "geweke",
                               "Heidelberger-Welch" = "heidel")),
      verbatimTextOutput(session$ns("diagnostics")),
      textExportButton(session$ns("exportText"))
    )
    )
  })
  } else {
    observeEvent(input$showDiag, {
      showModal(modalDialog(
        "Model Diagnostics",
        easyClose = TRUE,
        footer = modalButton("OK"),
        radioButtons(session$ns("diagType"), label = "Diagnostics Type",
                     choices = c("Gelman Scale Reduction Factor" = "gelman",
                                 "Raftery and Lewis" = "raftery",
                                 "Geweke z-Score" = "geweke",
                                 "Heidelberger-Welch" = "heidel")),
        verbatimTextOutput(session$ns("diagnostics")),
        textExportButton(session$ns("exportText"))
      )
      )
    })

  }

  printFunDiag <- reactive({
    validate(validInput(model()))
    if(choice){
      if(!is.null(input$modelChoice) && input$modelChoice != ""){
        modelPrint <- model()$models[[input$modelChoice]]
      } else {
        modelPrint <- model()$models[[1]]
      }
      modelPrint$model <- modelPrint
    } else {
      modelPrint <- model()
    }
    function() {

      parameters <- modelPrint$model$beta
      nChains <- modelPrint$nChains
      colnames(parameters) <- paste0("beta_", 1:ncol(parameters))
      parameters <- as.data.frame(parameters)
      diag <- convergenceDiagnostics(parameters, nChains)
      diagType <- input$diagType
      return(diag[[diagType]])
    }
  })

  output$diagnostics <- renderPrint({
    req(model())
    print(printFunDiag()())
  })

  callModule(textExport, "exportText", printFun = printFunDiag, filename = "diagnostics")
}

convergenceDiagnostics <- function(parameters, nChains){
  splitChains <- factor(rep(1:nChains, each = nrow(parameters) / nChains))

  mcmcObject <- split(parameters, splitChains)

  mcmcObject <- lapply(mcmcObject, function(x){
    x <- as.matrix(x)
    x <- mcmc(x, start = 1, end = nrow(x))
    x
  })

  raftery <- try({raftery.diag(parameters)}, silent = TRUE)
  gelman <- try({gelman.diag(mcmcObject, autoburnin = FALSE, multivariate = FALSE)}, silent = TRUE)
  geweke <- try({geweke.diag(mcmcObject)}, silent = TRUE)
  heidel <- try({heidel.diag(parameters)}, silent = TRUE)

  if(nChains == 1){
    gelman <- "For Gelman-Rubin diagnostics, at least 2 chains are required.
    Number of chains option available in the model options tab"
  }

  return(list(raftery = raftery, gelman = gelman, geweke = geweke, heidel = heidel))
}

textExportButton <- function(id, title = "Download") {
  ns <- NS(id)
  downloadButton(ns("download"), title)
}

textExport <- function(input, output, session, printFun, filename = "output") {
  content <- reactive({
    capture.output(printFun()())
  })

  output$text <- renderPrint({
    printFun()()
  })

  output$download <- downloadHandler(
    filename = function(){
      paste0(filename, ".txt")
    },
    content = function(file) {
      writeLines(content(), file)
    }
  )
}
