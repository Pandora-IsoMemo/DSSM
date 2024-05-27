#' Download DSSM Model UI
#'
#' @param ns Namespace
downloadDSSMModelUI <- function(ns) {
  tagList(
    checkboxInput(ns("useDownload"), label = "Download model"),
    conditionalPanel(
      ns = ns,
      condition = "input.useDownload == true",
      tags$hr(),
      pickerInput(
        inputId = ns("downloadSavedMaps"),
        label = "Include Saved Maps",
        choices = c("Please save maps first" = ""),
        selected = c("Please save maps first" = ""),
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `none-selected-text` = "No maps selected",
          `selected-text-format` = "count > 8"
        ),
        multiple = TRUE
      ),
      downloadModelUI(ns("modelDownload"), label = "Download"),
      tags$hr()
    ),
    tags$br()
  )
}

packModelForDownload <- function(Model, savedMaps, savedMapsIDs = "") {
  if (length(savedMapsIDs) == 0 ||
      (length(savedMapsIDs) == 1 && savedMapsIDs == "")) {
    res <- list(currentModel = Model,
                savedMaps = list())
  } else {
    res <- list(currentModel = Model,
                savedMaps = savedMaps[as.numeric(savedMapsIDs)])
  }

  return(res)
}

unpackModel <- function(uploadedModel) {
  if (all(names(uploadedModel) %in% c("currentModel", "savedMaps"))) {
    # new format, version >= 24.05.4
    res <- uploadedModel$currentModel
  } else {
    # old format
    res <- uploadedModel
  }

  return(res)
}

unpackSavedMaps <- function(uploadedModel, currentSavedMaps) {
  if (all(names(uploadedModel) %in% c("currentModel", "savedMaps"))) {
    # new format, version >= 23.05.4
    res <- uploadedModel$savedMaps %>%
      updateNameEntryIfDuplicate(oldList = currentSavedMaps,
                                 listType = "Saved map")
  } else {
    # old format, savedMaps were not included in downloads
    res <- list()
  }

  return(res)
}
