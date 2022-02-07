showMessage <- function(fun, msg = "Loading ...", ...) {
  force(fun)
  args <- c(list(...), message = msg)
  function(...) {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    do.call(progress$set, args)
    fun(...)
  }
}
