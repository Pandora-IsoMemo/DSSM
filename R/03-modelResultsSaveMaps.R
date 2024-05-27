createSavedMap <- function(model, predictions, plot, plotFUN, type, name) {
  tmp <- tempfile("plot", fileext = ".png")
  png(tmp, width = 800, height = 500)
  replayPlot(plot)
  dev.off()

  list(model = model, # required to use getThumbnail()
       predictions = predictions, # required to use createDifferenceMap()
       plotFUN = plotFUN, # required to use getThumbnail()
       file = tmp, # required to use getThumbnail()
       type = type, # required to filter for the model type
       name = name)
}

observeSavedMaps <- function(input, output, session, savedMaps, type) {
  observeEvent(savedMaps(), {
    choices <- getMapChoices(savedMaps(), type)

    updateSelectInput(session, "savedModel", choices = choices)

    # downloadSavedMaps pickerInput is hidden, so this is currently not needed
    #updatePickerInput(session, "downloadSavedMaps", choices = choices)
  })
}
