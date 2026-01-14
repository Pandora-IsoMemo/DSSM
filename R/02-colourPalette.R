colour_palette_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("Colours"), label = "Colour palette",
      choices = list(
        "Single-hue (sequential)" = list(
          "Red"    = "Reds",
          "Purple" = "Purples",
          "Orange" = "Oranges",
          "Grey"   = "Greys",
          "Blue"   = "Blues",
          "Green"  = "Greens"
        ),
        "Multi-hue (sequential)" = list(
          "Yellow-Red"   = "YlOrRd",
          "Purple-Red"   = "PuRd",
          "Yellow-Green" = "YlGn",
          "Red-Purple"   = "RdPu",
          "Orange-Red"   = "OrRd",
          "Green-Blue"   = "GnBu",
          "Blue-Green"   = "BuGn",
          "Purple-Blue"  = "PuBu"
        ),
        "Diverging" = list(
          "Red–Yellow–Green" = "RdYlGn",
          "Red–Yellow–Blue"  = "RdYlBu",
          "Red–Blue"         = "RdBu",
          "Purple–Green"     = "PRGn",
          "Pink–Green"       = "PiYG",
          "Brown–Green"      = "BrBG",
          "Purple–Orange"    = "PuOr"
        )
      ),
      selected = "RdYlGn"
    ),
    checkboxInput(ns("reverseCols"), label = "Reverse colors", value = FALSE, width = "100%"),
    checkboxInput(
      inputId = ns("smoothCols"), label = "Smooth color transition",
      value = FALSE, width = "100%"
    ),
    conditionalPanel(
      ns = ns,
      condition = "input.smoothCols == false",
      sliderInput(
        inputId = ns("ncol"), label = "Approximate number of colour levels",
        min = 4, max = 50, value = 50, step = 2, width = "100%"
      )
    )
  )
}

colour_palette_server <- function(id, fixCol) {
  moduleServer(
    id,
    function(input, output, session) {
      n_val <- reactive({
        if (input$smoothCols) {
          200
        } else if (isFALSE(fixCol())) {
          input$ncol
        } else {
          isolate(input$ncol)
        }
      })

      reactive(
        list(
          colours     = input$Colours,
          reverse     = input$reverseCols,
          n           = n_val()
        )
      )
    }
  )
}

extract_palette <- function(colors, ncolors, reverse = FALSE) {
  colors <- colorRampPalette(brewer.pal(9, colors))(ncolors)

  if (reverse) {
    colors <- rev(colors)
  }

  colors
}