createPointInputGroup <- function(df, ns) {
  lapply(seq_len(nrow(df)), function(i) {
    createPointInput(
      index = df$index[i],
      y = df$y[i],
      ymin = df$ymin[i],
      ymax = df$ymax[i],
      x = df$x[i],
      xmin = df$xmin[i],
      xmax = df$xmax[i],
      pointColor = df$pointColor[i],
      pointSize = df$pointSize[i],
      pointAlpha = df$pointAlpha[i],
      ns = ns
    )
  })
}

createPointInput <- function(index, y, x, ymin, ymax, xmin, xmax,
                             pointColor, pointSize, pointAlpha, ns) {
  tags$div(
    numericInput(
      ns(paste("yT", index, sep = "_")),
      paste("y", index, sep = "_"),
      value = y,
      min = 0,
      max = 1,
      step = 0.01
    ),
    numericInput(
      ns(paste("yminT", index, sep = "_")),
      paste("ymin", index, sep = "_"),
      value = ymin,
      min = 0,
      max = 1,
      step = 0.01
    ),
    numericInput(
      ns(paste("ymaxT", index, sep = "_")),
      paste("ymax", index, sep = "_"),
      value = ymax,
      min = 0,
      max = 1,
      step = 0.01
    ),
    numericInput(
      ns(paste("xT", index, sep = "_")),
      paste("x", index, sep = "_"),
      value = x,
      min = 0,
      max = 1,
      step = 0.01
    ),
    numericInput(
      ns(paste("xminT", index, sep = "_")),
      paste("xmin", index, sep = "_"),
      value = xmin,
      min = 0,
      max = 1,
      step = 0.01
    ),
    numericInput(
      ns(paste("xmaxT", index, sep = "_")),
      paste("xmax", index, sep = "_"),
      value = xmax,
      min = 0,
      max = 1,
      step = 0.01
    ),
    colourInput(ns(paste("pointColorT", index, sep = "_")),
                label = "Choose point color", value = pointColor),
    sliderInput(ns(paste("pointSizeT", index, sep = "_")),
                label = "Point size", value = pointSize, min = 0.1, max = 5, step = 0.1),
    sliderInput(ns(paste("pointAlphaT", index, sep = "_")),
                label = "Point alpha", value = pointAlpha, min = 0.1, max = 1, step = 0.1)
  )
}

createPointInputGroup2D <- function(df, ns) {
  lapply(seq_len(nrow(df)), function(i) {
    createPointInput2D(
      index = df$index[i],
      y = df$y[i],
      x = df$x[i],
      pointColor = df$pointColor[i],
      pointSize = df$pointSize[i],
      pointAlpha = df$pointAlpha[i],
      label = df$label[i],
      ns = ns
    )
  })
}

createPointInput2D <- function(index, y, x, ymin, ymax, xmin, xmax,
                             pointColor, pointSize, pointAlpha, label, ns) {
  tags$div(
    numericInput(
      ns(paste("y", index, sep = "_")),
      paste("y", index, sep = "_"),
      value = y,
      min = 0,
      max = 1,
      step = 0.01
    ),
    numericInput(
      ns(paste("x", index, sep = "_")),
      paste("x", index, sep = "_"),
      value = x,
      min = 0,
      max = 1,
      step = 0.01
    ),
    textInput(ns(paste("label", index, sep = "_")), paste("point label", index, sep = "_"),
              value = label),
    colourInput(ns(paste("pointColor", index, sep = "_")),
                label = "Choose point color", value = pointColor),
    sliderInput(ns(paste("pointSize", index, sep = "_")),
                label = "Point size", value = pointSize, min = 0.1, max = 5, step = 0.1),
    sliderInput(ns(paste("pointAlpha", index, sep = "_")),
                label = "Point alpha", value = pointAlpha, min = 0.1, max = 1, step = 0.1)
  )
}

