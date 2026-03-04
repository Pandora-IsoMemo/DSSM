batchModelingUI <- function(id) {
    ns <- NS(id)

    actionButton(ns("openDialog"), "Batch Modeling")
}

batchModelingDialog <- function(ns, variableNames) {
    modalDialog(
        title = "Batch Modeling",
        plotOutput(ns("plot")),
        pickerInput(ns("independentVariables"), variableNames, choices = NULL, multiple = TRUE),
        actionButton(ns("start"), "Start Batch Modeling"),
        textOutput(ns("info"))
    )
}

batchModeling <- function(input, output, session, data, plotFun, modelParams, type, savedMaps, estimateWrapper, variableNames = "Dependent Variables") {
    observeEvent(input$openDialog, {
        showModal(batchModelingDialog(ns = session$ns, variableNames = variableNames))
        updatePickerInput(session, "independentVariables", choices = names(data()))
    })

    Model <- reactiveVal(NULL)

    batchVariables <- reactiveVal(character(0))

    currentVariable <- reactiveVal(NULL)

    progress <- NULL

    observeEvent(input$start, {
        req(input$independentVariables)

        progress <<- shiny::Progress$new()

        progress$set(message = "Batch modeling", value = 0)

        currentVariable(input$independentVariables[1])
        batchVariables(input$independentVariables[-1])
    })

    observeEvent(currentVariable(), {
        logging("Estimating model for %s", currentVariable())
        progress$inc(
            1/length(input$independentVariables),
            detail = paste("Modeling variable", currentVariable())
        )

        params <- modelParams()
        params$IndependentX <- currentVariable()

        model <- try({
            estimateWrapper(data(), params)
        })
        if (inherits(model, "try-error")) {
            alert(paste("Could not model for variable ", currentVariable, ". Proceeding with next one."))
            if (length(batchVariables()) > 0) {
                logging("Setting next variable to %s", batchVariables()[1])
                currentVariable(batchVariables()[1])
                batchVariables(batchVariables()[-1])
            } else {
                logging("Batch modeling done. Setting next variable to NULL")
                progress$close()
                currentVariable(NULL)
            }
            Model(NULL)
            return()
        }
        Model(model)
    })

    values <- reactiveValues(updated = NULL)

    output$plot <- renderPlot({
        req(Model())
        logging("Rendering plot")
        validate(validInput(Model()))
        res <- plotFun()(Model())
        values$updated <- Sys.time()
        values$predictions <- res$XPred
        log_object_size(values$predictions)
        values$plot <- recordPlot()
        log_object_size(values$plot)
    })


    observe({
        req(values$updated)
        logging("Trying to save map and model")
        if (Sys.time() - values$updated < 10) {
            logging("Last update of plot too recent, trying again in 5 sec")
            invalidateLater(5000)
            return()
        }
        logging("Saving map and model")

        isolate({
            mapName <- currentVariable()

            map <- createSavedMap(
                model = Model(),
                predictions = values$predictions,
                plot = values$plot,
                plotFUN = plotFun(),
                type = type,
                name = mapName
            )

            maps <- savedMaps()
            maps[[length(maps) + 1]] <- map
            savedMaps(maps)

            if (length(batchVariables()) > 0) {
                logging("Setting next variable to %s", batchVariables()[1])
                currentVariable(batchVariables()[1])
                batchVariables(batchVariables()[-1])
            } else {
                logging("Batch modeling done. Setting next variable to NULL")
                progress$close()
                currentVariable(NULL)
            }
        })
    })

    Model
}
