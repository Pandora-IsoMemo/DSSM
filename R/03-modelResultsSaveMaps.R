createSavedMap <- function(model, predictions, plot, type, name) {
  tmp <- tempfile("plot", fileext = ".png")
  png(tmp, width = 800, height = 500)
  replayPlot(plot)
  dev.off()

  list(model = model, predictions = predictions, plot = plot, file = tmp , type = type, name = name)
}

observeSavedMaps <- function(input, output, session, savedMaps, type) {
  observeEvent(savedMaps(), {
    choices <- getMapChoices(savedMaps(), type)

    updateSelectInput(session, "savedModel", choices = choices)
    updatePickerInput(session, "downloadSavedMaps", choices = choices)
  })
}
