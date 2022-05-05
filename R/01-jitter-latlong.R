##########################################################################################
# The functions in this script were copied from the package "mullenMisc" with thanks to  #
# lmullen for providing them on github: https://github.com/lmullen/mullenMisc under the  #
# MIT + file LICENSE.                                                                    #
##########################################################################################

#' Jitter latitude and longitude coordinates
#'
#' Given a latitude or longitude coordinate, return that coordinate jittered
#' plus or minus a certain maximum amount. This function assumes a spherical
#' earth when calculating the maximum amount to jitter.
#'
#' @param coord The coordinate in degrees
#' @param type Whether the coordinate is latitude or longitude
#' @param latitude If the coordinate is longitude, then the latitude to use for
#'   calculating the maximum amount to jitter.
#' @param km The maximum number of kilometers to jitter a point plus or minus.
#' @return The jittered coordinate in degrees.
#' @examples
#' jitter_latlong(-73, type = "long", lat = 43, km = 1)
#' jitter_latlong(42, type = "lat", km = 1)
#' @export
#' @seealso \code{\link{length_of_degree}}
jitter_latlong <- function(coord, type = c("lat", "long"), latitude, km = 1) {
  type = match.arg(type)
  if(missing(latitude) & type == "lat") latitude <- coord
  km_per_degree <- length_of_degree(latitude, type = type)
  degree_per_km <- 1 / km_per_degree
  coord + (runif(1, min = -1, max = 1) * degree_per_km * km)
}
jitter_latlong <- Vectorize(jitter_latlong,
                            vectorize.args = c("coord", "latitude"))

#' Length of a degree of latitude or longitude
#'
#' Calculates the length of a degree of latitude or longitude in kilometers,
#' assuming an spherical earth.
#'
#' @param degree The degree to calculate the length for
#' @param type Whether to return the length of a degree for latitude or longitude
#' @return Length of the degree in kilometers
#'
#' @export
length_of_degree <- function(degree, type = c("lat", "long")) {
  type <- match.arg(type)
  length_at_equator <- 110.5742727 # in kilometers
  if (type == "long") {
    cos(degree * (2 * pi) / 360) * length_at_equator
  } else if (type == "lat") {
    length_at_equator
  }
}
