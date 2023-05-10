unpackModel <- function(uploadedModel) {
  if (all(names(uploadedModel) %in% c("currentModel", "savedMaps"))) {
    uploadedModel$currentModel
  } else {
    uploadedModel
  }
}

unpackSavedMaps <- function(uploadedModel, currentSavedMaps) {
  if (all(names(uploadedModel) %in% c("currentModel", "savedMaps"))) {
    uploadedModel$savedMaps %>%
      solveConflictsOfNames(oldList = currentSavedMaps,
                            listType = "Saved map")
  } else {
    list()
  }
}

solveConflictsOfNames <- function(newList, oldList, listType = "Saved map") {
  # check for name conflicts
  if (all(!(names(newList) %in% names(oldList)))) return(newList)

  nameExists <- which(names(newList) %in% names(oldList))
  shinyalert(
    title = "Duplicated names found.",
    text = sprintf(
      "'%s' name(s) \n %s \n already existed and had been replaced successfully.",
      listType,
      paste(names(newList)[nameExists], collapse = ", ")
    ),
    type = "warning"
  )

  # rename duplicated list names
  newNames <- names(newList)
  while (any(newNames %in% names(oldList))) {
    nameExists <- which(newNames %in% names(oldList))
    newNames[nameExists] <- lapply(newNames[nameExists], incIndexOfName)
    names(newList) <- newNames
  }

  return(newList)
}

#' Inc Index Of Name
#'
#' If the name has no index, add a new index: "(1)". If an index already exists, increase it by one.
#'
#' @param name (character) name
incIndexOfName <- function(name) {
  # extract index
  currentIndex <-
    regmatches(name, regexpr("\\([[:digit:]]+\\)$", name))

  # inc index
  if (length(currentIndex) == 0) {
    paste0(name, "(1)")
  } else {
    # get new index
    newIndex <- currentIndex %>%
      gsub(pattern = "\\(|\\)",
           replacement = "") %>%
      as.numeric() + 1

    # replace with new index
    gsub("\\([[:digit:]]+\\)$" ,
         paste0("(", newIndex, ")") ,
         name)
  }
}
