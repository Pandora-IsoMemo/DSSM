mapTable <- function(maps, ns) {
  tableRows <- lapply(seq_along(maps), function(i) {
    rr <- maps[[i]]
    tags$tr(
      tags$td(plotOutput(ns(paste0("thumbnail_", i)), width = 160, height = 100)),
      tags$td(rr$name),
      tags$td(getTabId(rr$type)),
      tags$td(
        tags$button("Delete Saved Map", onclick = jsSetInputValue(ns("deleteMap"), i))
      )
    )
  })


  tags$table(
    class = "table table-bordered table-saved-maps",
    tags$tr(
      tags$th("Thumbnail"),
      tags$th("Name"),
      tags$th("Type"),
      tags$th("Action")
    ),
    tableRows
  )
}

jsSetInputValue<- function(id, value) {
  paste0("Shiny.setInputValue('", id, "', {i:", value, ", rand: Math.random()});")
}
