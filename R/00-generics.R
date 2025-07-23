#' Generic function for setting values in an object
#'
#' @param obj An object of class ScaleBar or similar.
#' @param ... Additional arguments.
#'
#' @export
set <- function(obj, ...) {
  UseMethod("set")
}
