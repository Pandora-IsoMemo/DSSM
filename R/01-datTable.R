#' Create data table
#'
#' @param dat raw data
#' @param columns which columns should be shown?
#' @export

datTable <- function(dat, columns = names(dat)){
  if (nrow(dat) == 0)
    return(NULL)
  if (is.null(dat))
    return(NULL)

  dat <- dat[names(dat) %in% columns]

  descCol <- which(colnames(dat) == "description")

  columnDefs <- if (length(descCol) == 0) NULL
  else list(list(className = "cell-pointer", targets = descCol - 1))

  DT::datatable(
    data.frame(dat),
    rownames = FALSE,
    #escape = FALSE,
    filter = "top",
    style = "bootstrap",
    options = list(
      columnDefs = columnDefs,
      pageLength = 25
    ),
    selection = list(mode = 'single', target = 'cell')
  )
}

categoryChoices <- function(mapping) {
  unique(mapping[! mapping$shiny %in% columnDefault(), ]$category)
}

columnChoices <- function(category, mapping, cal = FALSE){

  columns <- setdiff(mapping$shiny[ which(mapping$category %in% category) ], columnDefault())
  if (!cal) columns <- columns[!grepl("_cal", columns)]
  columns
}

columnDefault <- function(){
  c("source", "id")
}

getDataColumns <- function(mapping, input){

  cats <- gsub(" ", "", paste0("selectCategory", categoryChoices(mapping)))
  cats <- cats[sapply(cats, function(x) isTRUE(input[[x]]))]
  cols <- gsub("Category", "Columns", cats)

  c(columnDefault(), unlist(lapply(cols, function(x) input[[x]])))

}


