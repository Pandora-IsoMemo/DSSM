#' Generic function for setting values in an object
#'
#' @param obj An object of class ScaleBar or similar.
#' @param ... Additional arguments.
#'
#' @export
set <- function(obj, ...) {
  UseMethod("set")
}

#' Add ocean layer (generic)
#'
#' Generic for adding an ocean layer to a map plot.
#'
#' @param x A \code{MapLayers} object.
#' @param ... Further arguments passed to methods.
#' @export
add_ocean <- function(x, ...) UseMethod("add_ocean")

#' Add land layer (generic)
#'
#' Generic for adding a land layer to a map plot.
#'
#' @param x A \code{MapLayers} object.
#' @param ... Further arguments passed to methods.
#' @export
add_land <- function(x, ...) UseMethod("add_land")

#' Add coastline layer (generic)
#'
#' Generic for adding a coastline layer to a map plot.
#'
#' @param x A \code{MapLayers} object.
#' @param ... Further arguments passed to methods.
#' @export
add_coast <- function(x, ...) UseMethod("add_coast")

#' Add grid lines layer (generic)
#'
#' Generic for adding grid lines to a map plot.
#'
#' @param x A \code{MapLayers} object.
#' @param ... Further arguments passed to methods.
#' @export
add_grids <- function(x, ...) UseMethod("add_grids")

#' Add borders layer (generic)
#'
#' Generic for adding political/country borders to a map plot.
#'
#' @param x A \code{MapLayers} object.
#' @param ... Further arguments passed to methods.
#' @export
add_borders <- function(x, ...) UseMethod("add_borders")

#' Add center line (generic)
#'
#' Generic for adding a center meridian line to a map plot.
#'
#' @param x A \code{MapLayers} object.
#' @param ... Further arguments passed to methods.
#' @export
add_center_line <- function(x, ...) UseMethod("add_center_line")

