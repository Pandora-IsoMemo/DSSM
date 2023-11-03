#' Create data table
#'
#' @param dat raw data
#' @param columns which columns should be shown?
#' @export

datTable <- function(dat, columns = names(dat)){
  if (is.null(dat) || length(dat) == 0)
    return(dat)

  if (nrow(dat) == 0)
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
  if (length(mapping) == 0) return(c())
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

#' Extract Choices From Isomemo Api
#'
#' @param apiOutput output from the isomemo api
#'
#' @export
extractChoicesFromIsomemoApi <- function(apiOutput) {
  if (is.null(apiOutput) || (length(apiOutput) == 0 && is.null(attr(apiOutput, "error")))) {
    choices <- c("No API output" = "")
  } else if (length(apiOutput) == 0 && !is.null(attr(apiOutput, "error"))) {
    choices <- c("")
    names(choices) <- attr(apiOutput, "error")
    } else {
      choices <- apiOutput

      if ("IsoMemo" %in% choices) {
        # rename label
        namesChoices <- names(choices)
        namesChoices[choices == "IsoMemo"] <- "IsoMemo - Humans"
        names(choices) <- namesChoices
      }
    }

  choices
}

