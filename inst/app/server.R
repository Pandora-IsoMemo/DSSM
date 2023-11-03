library(MpiIsoApp)

# annoyingly calling mclust::Mclust yields an error
# Error in mclustBIC: could not find function "mclustBIC"
# Therefore we need to import and load the whole package mclust
library(mclust)
library(yaml)

options(shiny.maxRequestSize = 200*1024^2)
options(scipen=999)

# load config variables
configFile <- system.file("config.yaml", package = "MpiIsoApp")
appConfig <- yaml::read_yaml(configFile)

server <- function(input, output, session) {
#  savedMaps <- reactiveVal(readRDS("~/savedMaps.rds"))
  savedMaps <- reactiveVal(list())
  fruitsData <- reactiveVal(list(event = NULL, data = NULL))

  isoData <- dataExplorerServer("dataExplorer", config = appConfig)
  callModule(interactiveMap, "interactivemap", isoData = isoData)

  if (!isOnlyDataSearchMode()) {
    callModule(modelResults2D, "model2D", isoData = isoData, savedMaps = savedMaps,
               fruitsData = fruitsData, config = appConfig)
    callModule(modelResults3D, "model3D", isoData = isoData, savedMaps = savedMaps,
               fruitsData = fruitsData, config = appConfig)
    callModule(modelResultsSpread, "spread", isoData = isoData, savedMaps = savedMaps,
               fruitsData = fruitsData, config = appConfig)
    callModule(modelResults2DKernel, "model2DKernel", isoData = isoData, savedMaps = savedMaps,
               fruitsData = fruitsData, config = appConfig)
    callModule(modelResults3DKernel, "model3DKernel", isoData = isoData, savedMaps = savedMaps,
               fruitsData = fruitsData, config = appConfig)
    callModule(mapDiff, "difference", savedMaps = savedMaps,
               fruitsData = fruitsData, config = appConfig)
    callModule(mapSim, "similarity", savedMaps = savedMaps,
               fruitsData = fruitsData, config = appConfig)
    callModule(modelResultsAssign, "assign", isoData = isoData, config = appConfig)

    callModule(savedMapsTab, "svmt", savedMaps = savedMaps)
  }

  if (reSourcesInstalled()) {
    callModule(ReSources::fruitsTab, "fruits", isoMemoData = fruitsData)
  }

  observeEvent(input$getHelp, {
    showModal(modalDialog(
      title = "Help",
      easyClose = TRUE,
      getHelp(input$tab)
    ))
  })

  observe({
    query <- session$clientData$url_search
    params <- shiny::parseQueryString(query)

    if (!is.null(params$skin) && params$skin %in% allowedSkins()) {
      setSkin(params$skin)
      shiny::updateRadioButtons(session, "skin", selected = params$skin)
    }
  })

  observeEvent(input$skin, {
    setSkin(input$skin)

    req(input$skin == "isomemo")
    if (!DataTools::has_internet()) {
      databaseChoices <- mappingChoises <- "No internet connection ..."
    } else {
      databaseChoices <- extractChoicesFromIsomemoApi(
        IsoMemo::getDatabaseList(mapping = input[["dataExplorer-mappingId"]])
      )
      mappingChoises <- extractChoicesFromIsomemoApi(
        IsoMemo::getMappings()
      )
    }

    updateSelectInput(session, "dataExplorer-mappingId", choices = mappingChoises)

    shinyWidgets::updatePickerInput(session, "dataExplorer-database", choices = databaseChoices)

  })
}
