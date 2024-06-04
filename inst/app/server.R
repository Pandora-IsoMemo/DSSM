library(DSSM)

# annoyingly calling mclust::Mclust yields an error
# Error in mclustBIC: could not find function "mclustBIC"
# Therefore we need to import and load the whole package mclust
library(mclust)

options(shiny.maxRequestSize = 300*1024^2)
options(scipen=999)

server <- function(input, output, session) {
#  savedMaps <- reactiveVal(readRDS("~/savedMaps.rds"))
  savedMaps <- reactiveVal(list())
  fruitsData <- reactiveVal(list(event = NULL, data = NULL))

  isoData <- dataExplorerServer("dataExplorer")
  callModule(interactiveMap, "interactivemap", isoData = isoData)

  if (!isOnlyDataSearchMode()) {
    callModule(modelResults2D, "model2D", isoData = isoData, savedMaps = savedMaps,
               fruitsData = fruitsData)
    callModule(modelResults3D, "model3D", isoData = isoData, savedMaps = savedMaps,
               fruitsData = fruitsData)
    callModule(modelResultsSpread, "spread", isoData = isoData, savedMaps = savedMaps,
               fruitsData = fruitsData)
    callModule(modelResults2DKernel, "model2DKernel", isoData = isoData, savedMaps = savedMaps,
               fruitsData = fruitsData)
    callModule(modelResults3DKernel, "model3DKernel", isoData = isoData, savedMaps = savedMaps,
               fruitsData = fruitsData)
    callModule(mapDiff, "difference", savedMaps = savedMaps,
               fruitsData = fruitsData)
    callModule(mapSim, "similarity", savedMaps = savedMaps,
               fruitsData = fruitsData)
    callModule(modelResultsAssign, "assign", isoData = isoData)

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
      mappingChoises <- databaseChoices <- "No internet connection ..."
      updateSelectInput(session, "dataExplorer-mappingId", choices = mappingChoises)
    } else {
      mappingChoises <- extractChoicesFromIsomemoApi(IsoMemo::getMappings())
      databaseChoices <- extractChoicesFromIsomemoApi(
        IsoMemo::getDatabaseList(mapping = mappingChoises[[1]])
      )
      updateSelectInput(session,
                        "dataExplorer-mappingId",
                        choices = mappingChoises,
                        selected = mappingChoises[[1]])
    }

    shinyWidgets::updatePickerInput(session, "dataExplorer-database", choices = databaseChoices)
  })
}
