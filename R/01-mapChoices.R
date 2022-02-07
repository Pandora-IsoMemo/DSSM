getMapChoices <- function(maps, type) {
  choices <- unlist(lapply(seq_along(maps), function(i) { if(maps[[i]]$type %in% type) i else NULL }))
  names(choices) <- unlist(lapply(seq_along(maps), function(i) { if(maps[[i]]$type %in% type) maps[[i]]$name else NULL }))
  choices
}
