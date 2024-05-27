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
  if (savedMapsIDs == "") return(list(currentModel = Model))

  return(list(currentModel = Model,
              savedMaps = savedMaps[[as.numeric(savedMapsIDs)]]))
}

unpackModel <- function(uploadedModel) {
  if (all(names(uploadedModel) %in% c("currentModel", "savedMaps"))) {
    # new format, version >= 24.05.4
    uploadedModel$currentModel
  } else {
    # old format
    uploadedModel
  }
}

unpackSavedMaps <- function(uploadedModel, currentSavedMaps) {
  if (all(names(uploadedModel) %in% c("currentModel", "savedMaps"))) {
    # new format, version >= 23.05.4
    uploadedModel$savedMaps %>%
      updateNameEntryIfDuplicate(oldList = currentSavedMaps,
                                 listType = "Saved map")
  } else {
    # old format, savedMaps were not included in downloads
    list()
  }
}
