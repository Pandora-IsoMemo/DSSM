moveButtons <- function(ns) {
  buttonTable(ns)
}

buttonLayout <- function() {
  list(
    list(NA, up = "up", NA),
    list(left = "left", center = "center", right = "right"),
    list(NA, down = "down", NA)
  )
}

buttonTable <- function(ns) {
  tags$table(lapply(buttonLayout(), buttonRow, ns = ns))
}

buttonRow <- function(row, ns) {
  tags$tr(mapply(buttonCell, content = unname(row), id = names(row), MoreArgs = list(ns = ns), SIMPLIFY = FALSE))
}

buttonCell <- function(content, id, ns) {
  if (is.na(content)) tags$td()
  else tags$td(
    shiny::actionButton(inputId = ns(id), label = content)
  )
}
