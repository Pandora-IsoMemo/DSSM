getMapChoices <- function(maps,
                          type = c(
                            "localAvg",
                            "temporalAvg",
                            "spread",
                            "difference",
                            "similarity",
                            "kernel2d",
                            "kernel3d",
                            "user"
                          )) {
  if (length(maps) == 0)
    return(c("Please save maps first ..." = ""))

  choices <- unlist(lapply(seq_along(maps), function(i) {
    if (maps[[i]]$type %in% type)
      i
    else
      NULL
  }))
  names(choices) <- unlist(lapply(seq_along(maps), function(i) {
    if (maps[[i]]$type %in% type)
      maps[[i]]$name
    else
      NULL
  }))
  choices
}
