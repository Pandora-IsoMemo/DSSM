colour_palette_ui <- function(id, selected) {
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("Colours"), label = "Colour palette",
      choices = list(
        "Single colour" = list(
          "Red"    = "Reds",
          "Purple" = "Purples",
          "Orange" = "Oranges",
          "Grey"   = "Greys",
          "Blue"   = "Blues",
          "Green"  = "Greens"
        ),
        "Multi colour" = list(
          "Yellow-Red"   = "YlOrRd",
          "Purple-Red"   = "PuRd",
          "Yellow-Green" = "YlGn",
          "Red-Purple"   = "RdPu",
          "Orange-Red"   = "OrRd",
          "Green-Blue"   = "GnBu",
          "Blue-Green"   = "BuGn",
          "Purple-Blue"  = "PuBu"
        ),
        "White to multi colour" = list(
          "White-Yellow-Red"   = "WhYlRd",
          "White-Purple-Red"   = "WhPuRd",
          "White-Yellow-Green" = "WhYlGn",
          "White-Red-Purple"   = "WhRdPu",
          "White-Orange-Red"   = "WhOrRd",
          "White-Green-Blue"   = "WhGnBu",
          "White-Blue-Green"   = "WhBuGn",
          "White-Purple-Blue"  = "WhPuBu"
        ),
        "Diverging" = list(
          "Red-Yellow-Green" = "RdYlGn",
          "Red-Yellow-Blue"  = "RdYlBu",
          "Red-Blue"         = "RdBu",
          "Purple-Green"     = "PRGn",
          "Pink-Green"       = "PiYG",
          "Brown-Green"      = "BrBG",
          "Purple-Orange"    = "PuOr"
        )
      ),
      selected = selected, width = "100%"
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

.single_hue_palettes <- c(
  "Reds", "Purples", "Oranges", "Greys", "Blues", "Greens"
)

.diverging_palettes <- c(
  "RdYlGn", "RdYlBu", "RdBu", "PRGn", "PiYG", "BrBG", "PuOr"
)

.white_anchored_palettes <- list(
  "WhYlRd" = "YlOrRd",
  "WhPuRd" = "PuRd",
  "WhYlGn" = "YlGn",
  "WhRdPu" = "RdPu",
  "WhOrRd" = "OrRd",
  "WhGnBu" = "GnBu",
  "WhBuGn" = "BuGn",
  "WhPuBu" = "PuBu"
)

extract_palette <- function(pal_id, ncolors, reverse = FALSE) {
  if (pal_id %in% names(.white_anchored_palettes)) {
    # white-anchored multi-colour palettes (sequential)
    base_pal <- .white_anchored_palettes[[pal_id]]
    pal <- c("#FFFFFF", colorRampPalette(brewer.pal(9, base_pal))(ncolors - 1))
  } else if (pal_id %in% .single_hue_palettes) {
    # take only the darker end of the Brewer palette for single colours
    base_pal <- brewer.pal(9, pal_id)[-1]
    pal <- colorRampPalette(c("white", base_pal))(ncolors)
  } else if (pal_id %in% .diverging_palettes) {
    # ensure odd number of colors for diverging palettes
    ncolors <- if (ncolors %% 2 == 0) ncolors + 1 else ncolors
    pal <- colorRampPalette(brewer.pal(11, pal_id))(ncolors)
  } else {
    # other sequential palettes
    pal <- colorRampPalette(brewer.pal(9, pal_id))(ncolors)
  }

  if (reverse) rev(pal) else pal
}