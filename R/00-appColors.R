# The colors are from shinydashboard:::validColors
# See https://rstudio.github.io/shinydashboard/appearance.html#statuses-and-colors

appColors <- function(colors = c("lightBlue", "red", "green", "orange", "teal",
                                 "yellow", "aqua", "blue", "navy", "olive", "lime",
                                 "fuchsia", "purple", "maroon", "black"),
                      names = TRUE){
  colorDef <- c(lightBlue = "#3C8DBC",
                orange = "#FF851B",
                green = "#00A65A",
                red = "#DD4B39",
                teal = "#39CCCC",
                yellow = "#F39C12",
                aqua = "#00C0EF",
                blue = "#0073B7",
                navy = "#001F3F",
                olive = "#3D9970",
                lime = "#01FF70",
                fuchsia = "#F012BE",
                purple = "#605CA8",
                maroon = "#D81B60",
                black = "#111111"
  )

  if (names){
    colorDef[colors]
  } else if (!names){
    as.vector(colorDef[colors])
  }



}
