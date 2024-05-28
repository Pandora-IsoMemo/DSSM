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
      # # hide option to download savedMaps, objects are too big and the app becomes too slow after uploading them
      # pickerInput(
      #   inputId = ns("downloadSavedMaps"),
      #   label = "Include Saved Maps",
      #   choices = c("Please save maps first" = ""),
      #   selected = c("Please save maps first" = ""),
      #   options = list(
      #     `actions-box` = TRUE,
      #     size = 10,
      #     `none-selected-text` = "No maps selected",
      #     `selected-text-format` = "count > 8"
      #   ),
      #   multiple = TRUE
      # ),
      downloadModelUI(ns("modelDownload"), label = "Download"),
      tags$hr()
    ),
    tags$br()
  )
}

downloadDSSMModel <- function(input, output, session, dat, model, #savedMaps,
                              subFolder, tabId, uploadedNotes) {
  downloadModelServer("modelDownload",
                      dat = dat,
                      inputs = input,
                      model = reactive(packModelForDownload(
                        model#, savedMaps, savedMapsIDs = input[["downloadSavedMaps"]]
                      )),
                      rPackageName = config()[["rPackageName"]],
                      subFolder = subFolder,
                      fileExtension = config()[["fileExtension"]],
                      helpHTML = getHelp(id = tabId),
                      modelNotes = uploadedNotes,
                      triggerUpdate = reactive(TRUE),
                      compressionLevel = 1)
}

packModelForDownload <- function(Model#, savedMaps, savedMapsIDs = ""
                                 ) {
  # # hide option to download savedMaps, objects are too big and the app becomes too slow after uploading them
  # if (length(savedMapsIDs) == 0 ||
  #     (length(savedMapsIDs) == 1 && savedMapsIDs == "")) {
  #   # return empty savedMaps
  #   res <- list(currentModel = Model,
  #               savedMaps = list())
  # } else {
  #   # return savedMaps
  #   res <- list(currentModel = Model,
  #               savedMaps = savedMaps[as.numeric(savedMapsIDs)])
  # }

  # only download model
  res <- list(currentModel = Model)

  return(res)
}

unpackModel <- function(uploadedModel) {
  if (any(names(uploadedModel) == "currentModel")) {
    # new format, version >= 24.05.4
    res <- uploadedModel$currentModel
  } else {
    # old format
    res <- uploadedModel
  }

  return(res)
}

unpackSavedMaps <- function(uploadedModel, currentSavedMaps) {
  # # disabling option to upload savedMaps, objects are too big and the app becomes too slow after uploading them
  # if (any(names(uploadedModel) == "savedMaps")) {
  #   # new format, version >= 23.05.4
  #   res <- uploadedModel$savedMaps %>%
  #     updateNameEntryIfDuplicate(oldList = currentSavedMaps,
  #                                listType = "Saved map")
  # } else {
  #   # old format, savedMaps were not included in downloads
  #   res <- list()
  # }

  res <- list()

  return(res)
}
