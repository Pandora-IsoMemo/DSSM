createSavedMap <- function(model, predictions, plot, type, name) {
  tmp <- tempfile("plot", fileext = ".png")
  png(tmp, width = 800, height = 500)
  replayPlot(plot)
  dev.off()

  list(model = model, predictions = predictions, file = tmp , type = type, name = name)
}
