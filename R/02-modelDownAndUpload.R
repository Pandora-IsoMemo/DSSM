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
      checkboxInput(ns("includeSavedMaps"), label = "Include all saved maps", value = FALSE),
      downloadModelUI(ns("modelDownload"), label = "Download"),
      tags$hr()
    ),
    tags$br()
  )
}

packModelForDownload <- function(Model, savedMaps, includeSavedMaps = FALSE) {
  if (includeSavedMaps) {
    list(currentModel = Model,
         savedMaps = savedMaps)
  } else {
    list(currentModel = Model)
  }
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
