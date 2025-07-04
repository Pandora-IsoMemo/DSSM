library(DSSM)

tagList(
  shiny::navbarPage(
    title = getAppTitle(),
    theme = shinythemes::shinytheme("flatly"),
    position = "fixed-top",
    collapsible = TRUE,
    id = "tab",
    dataExplorerUI("dataExplorer", "Data"),
    interactiveMapUI("interactivemap", "Interactive map"),
    if (!isOnlyDataSearchMode()) navbarMenu("Modeling",
                                            modelResults2DUI("model2D", "AverageR"),
                                            modelResults3DUI("model3D", "TimeR"),
                                            modelResultsSpreadUI("spread", "SpreadR"),
                                            modelResults2DKernelUI("model2DKernel", "KernelR"),
                                            modelResults3DKernelUI("model3DKernel", "KernelTimeR"),
                                            modelResultsDiffUI("difference", "OperatoR"),
                                            modelResultsSimUI("similarity", "LocateR"),
                                            modelResultsAssignUI("assign", "AssignR")
    ) else NULL,
    if (!isOnlyDataSearchMode()) savedMapsTabUI("svmt", "Saved maps") else NULL,
    if (isOnlyDataSearchMode()) modelLinkUI("modelLink", title = "Modeling") else NULL,
    if (reSourcesInstalled()) ReSources::fruitsUI("fruits", "ReSources") else NULL
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "IsoMemo/custom.css"),
    tags$script(src = "IsoMemo/custom.js"),
    tags$script(src = "IsoMemo/shinyMatrix.js"),
    if (reSourcesInstalled()) tagList(
      tags$link(rel = "stylesheet", type = "text/css", href = "ReSources/custom.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "ReSources/spreadingTable.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "ReSources/removeName.css"),
      tags$script(src = "ReSources/spreadingTable.js"),
      tags$script(src = "ReSources/shinyMatrix.js"),
      tags$script(src = "ReSources/priors.js"),
      tags$script(src = "ReSources/userEstimates.js"),
      tags$script(src = "ReSources/removeName.js")
    ) else NULL
  ),
  shinyTools::headerButtonsUI(id = "header", help_link = "https://pandora-isomemo.github.io/DSSM/articles/how-to-use-DSSM.html"),
  shinyjs::useShinyjs()
)
