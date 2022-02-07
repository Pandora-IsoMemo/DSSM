clickElement <- function(id, delay){
  js <- paste0("$('#", id, "').click();")

  if (delay)
    js <- paste0("setTimeout(function(){ ", js, "}, ", delay, ");")

  runjs(js)
}
