# Minimal plot function
dummy_plot <- function(model, time) {
  plot(1:10, main = paste("Time =", time))
}

# Dummy model
dummy_model <- list(a = 1)

test_that("new_PlotSeriesExport creates valid object", {
  obj <- new_PlotSeriesExport(
    plotFun = dummy_plot,
    Model = dummy_model,
    times = c(1, 2),
    exportType = "png",
    modelType = "dummy"
  )

  expect_s3_class(obj, "PlotSeriesExport")
  expect_equal(obj$status, "initialized")
  expect_true(dir.exists(obj$path))
  expect_equal(length(obj$mainFileNames), 2)
  expect_true(all(file.exists(dirname(obj$mainFileNames))))
})

test_that("generate.PlotSeriesExport creates plot files", {
  obj <- new_PlotSeriesExport(
    plotFun = dummy_plot,
    Model = dummy_model,
    times = c(1, 2),
    exportType = "png"
  )

  obj <- generate(obj)

  expect_equal(obj$status, "completed")
  expect_true(all(file.exists(obj$mainFileNames)))
  expect_true(all(file.exists(obj$pngFileNames)))
})

test_that("cleanup.PlotSeriesExport removes temporary directory", {
  obj <- new_PlotSeriesExport(
    plotFun = dummy_plot,
    Model = dummy_model,
    times = 1
  )
  expect_true(dir.exists(obj$path))
  obj <- cleanup(obj)
  expect_false(dir.exists(obj$path))
  expect_equal(obj$status, "cleaned")
})

test_that("exportSeries.PlotSeriesExport creates ZIP file", {
  obj <- new_PlotSeriesExport(
    plotFun = dummy_plot,
    Model = dummy_model,
    times = 1,
    modelType = "test"
  )
  obj <- generate(obj)

  zip_path <- tempfile(fileext = ".zip")
  exportSeries.PlotSeriesExport(obj, file = zip_path, modelType = "test", typeOfSeries = "onlyZip")

  expect_true(file.exists(zip_path))
  expect_gt(file.info(zip_path)$size, 0)
})

test_that("exportSeries.PlotSeriesExport creates GIF file", {
  obj <- new_PlotSeriesExport(
    plotFun = dummy_plot,
    Model = dummy_model,
    times = 1:2,
    modelType = "test"
  )
  generate(obj)

  gif_path <- tempfile(fileext = ".gif")
  exportSeries.PlotSeriesExport(obj, file = gif_path, modelType = "test", typeOfSeries = "onlyGif", fpsGif = 2)

  expect_true(file.exists(gif_path))
  expect_gt(file.info(gif_path)$size, 0)
})

test_that("exportMapR.PlotSeriesExport creates ZIP with image list JSON", {
  obj <- new_PlotSeriesExport(
    plotFun = dummy_plot,
    Model = dummy_model,
    times = 1:2
  )
  generate(obj)

  input <- list(
    `mapr-group` = "Group A",
    `mapr-variable` = "Variable X",
    `mapr-measure` = "Measure Y",
    `mapr-measureunit` = "unit"
  )

  zip_path <- tempfile(fileext = ".zip")
  exportMapR.PlotSeriesExport(obj, file = zip_path, input = input)

  expect_true(file.exists(zip_path))
  expect_gt(file.info(zip_path)$size, 0)
})
