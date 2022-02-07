sidebarPlotOutput <- function(id, condition){
  ns <- NS(id)

  conditionalPanel(
    condition = condition,
    h4(textOutput(ns("variable"))),
    div(
      class = "sidebar-plot-controls",
      selectInput(
        ns("type"),
        "Plot type",
        choices = NULL,
        selected = NULL
      ),
      div(
        actionButton(
          ns("export"),
          "Export",
          class = "copy-clipboard-button"
        )
      )
    ),
    plotOutput(ns("plot"), height = "200"),
    div(
      class = "clipboard",
      div(
        textAreaInput(ns("clipboard"), "Clipboard")
      ),
      actionButton(
        ns("copyClipboard"),
        "Copy to Clipboard"
      ),
      uiOutput(ns("stats"))
    )
  )
}


sidebarPlot <- function(input, output, session, x, y = NULL, nameX, nameY = NULL){
  mode <- reactive({
    ifelse(is.null(y), "univariate", "bivariate")
  })

  output$variable <- renderText({
    switch(
      mode(),
      univariate = nameX(),
      bivariate = paste(nameX(), nameY(), sep = " vs. ")
    )
  })

  output$plot <- renderPlot({
    switch(
      mode(),
      univariate = plotUnivariate(input$type, x(), nameX()),
      bivariate = plotBivariate(input$type, x(), y(), nameX(), nameY())
    )
  })

  output$stats <- renderUI({
    stats <- switch(
      mode(),
      univariate = statsUnivariate(x()),
      bivariate = statsBivariate(x(), y())
    )
    htmlStatsList(stats)
  })

  observe({
    choices <- switch(
      mode(),
      univariate = c("Histogram" = "hist", "Boxplot" = "boxplot", "KDE" = "kde"),
      bivariate = c("Scatterplot" = "scatter", "2D-KDE" = "kde2D")
    )

    updateSelectInput(session, "type", choices = choices)
  })

  observeEvent(input$export, {
    showModal(modalDialog(
      title = "Export Graphic",
      footer = modalButton("OK"),
      selectInput(session$ns("exportType"), "Filetype",
                  choices = c("png", "pdf", "svg", "tiff")),
      downloadButton(session$ns("exportExecute"), "Export"),
      easyClose = TRUE
    ))
  })

  output$exportExecute <- downloadHandler(
    filename = function(){
      switch(
        mode(),
        univariate = paste0(input$type, "-", nameX(), ".", input$exportType),
        bivariate = paste0(input$type, "-", nameX(), "-", nameY(), ".", input$exportType)
      )
    },
    content = function(file){
      switch(
        input$exportType,
        png = png(file),
        pdf = pdf(file),
        tiff = tiff(file),
        svg = svg(file)
      )

      switch(
        mode(),
        univariate = plotUnivariate(input$type, x(), nameX()),
        bivariate = plotBivariate(input$type, x(), y(), nameX(), nameY())
      )

      dev.off()
    }
  )

  observe({
    stats <- switch(
      mode(),
      univariate = statsUnivariate(x()),
      bivariate = statsBivariate(x(), y())
    )

    if (!is.null(stats)){
      updateTextAreaInput(
        session,
        "clipboard",
        value = statsRaw(stats)
      )
    }
  })
}

htmlStatsList <- function(l, digits = 2){
  if (is.null(l))
    return(NULL)

  l <- lapply(l, round, digits = digits)

  div(
    class = "stats-list",
    mapply(
      function(label, item){
        div(
          div(label, class = "stats-label"),
          div(item, class = "stats-item")
        )
      },
      names(l),
      unlist(l, use.names = FALSE),
      SIMPLIFY = FALSE
    )
  )
}

statsRaw <- function(l, digits = 2){
  l <- lapply(l, round, digits = digits)

  paste(
    mapply(
      function(label, item){
          paste(label, item, sep = "\t")
      },
      names(l),
      unlist(l, use.names = FALSE),
      SIMPLIFY = FALSE
    ),
    collapse = "\n"
  )
}

plotUnivariate <- function(type, x, name){
  if (is.null(x))
    return(NULL)

  x <- na.omit(x)

  if (length(x) == 0)
    return(NULL)

  if (type == "hist")
    hist(
      x,
      main = paste("Histogram of", name),
      xlab = name,
      ylab = "Frequency"
    )

  if (type == "boxplot"){
    par(mar = c(2.1, 4.1, 4.1, 2.1))
    boxplot(
      x,
      main = paste("Boxplot of", name),
      ylab = name
    )
    par(mar = c(5.1, 4.1, 4.1, 2.1))
  }

  if (type == "kde"){
    dens <- density(x)
    plot(
      dens,
      main = paste("KDE of", name)
    )
  }
}

plotBivariate <- function(type, x, y, nameX, nameY){
  if (is.null(x) | is.null(y))
    return(NULL)

  omit <- is.na(x) | is.na(y)
  x <- x[!omit]
  y <- y[!omit]

  if (length(x) == 0)
    return(NULL)

  if (type == "scatter"){
    plot(
      x, y,
      main = paste("Plot of", nameX, "vs.", nameY),
      xlab = nameX,
      ylab = nameY
    )
  }

  if (type == "kde2D"){
    kde <- kde2d(x, y, n = 250)
    image(kde, col = c("#FFFFFF", brewer.pal(9, "Blues")),
      main = paste("Plot of", nameX, "vs.", nameY),
      xlab = nameX,
      ylab = nameY
    )
  }

}

statsUnivariate <- function(x){
  if (is.null(x))
    return(NULL)

  x <- na.omit(x)

  list(
    mean = mean(x),
    sd = sd(x),
    min = min(x),
    "1st quartile" = quantile(x, 0.25),
    median = median(x),
    "3rd quartile" = quantile(x, 0.75),
    max = max(x)
  )
}

statsBivariate <- function(x, y){
  if (is.null(x) | is.null(y))
    return(NULL)

  omit <- is.na(x) | is.na(y)
  x <- x[!omit]
  y <- y[!omit]

  if (length(x) == 0)
    return(NULL)

  list(
    correlation = cor(x, y)
  )
}
