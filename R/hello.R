# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' Calculate Geographical Distance
#'
#' This function calculates the distance between two points specified by their longitude and latitude.
#' The result is given in kilometers.
#'
#' @param lon1 Longitude of the first point.
#' @param lat1 Latitude of the first point.
#' @param lon2 Longitude of the second point.
#' @param lat2 Latitude of the second point.
#' @return The distance between the two points in kilometers.
#' @examples
#' geo_distance(-74.006, 40.7128, -118.2437, 34.0522)
#'
geo_distance <- function(lon1, lat1, lon2, lat2) {
  R <- 6371 # Earth's radius in kilometers
  delta_lon <- deg_to_rad(lon2 - lon1)
  delta_lat <- deg_to_rad(lat2 - lat1)
  a <- sin(delta_lat / 2) ^ 2 + cos(deg_to_rad(lat1)) * cos(deg_to_rad(lat2)) * sin(delta_lon / 2) ^ 2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  return(R * c)
}

#' Convert Degrees to Radians
#'
#' This function converts an angle from degrees to radians.
#'
#' @param deg Angle in degrees.
#' @return Angle in radians.
#' @examples
#' deg_to_rad(180)
deg_to_rad <- function(deg) {
  return(deg * pi / 180)
}

#' Convert Radians to Degrees
#'
#' This function converts an angle from radians to degrees.
#'
#' @param rad Angle in radians.
#' @return Angle in degrees.
#' @examples
#' rad_to_deg(pi)
rad_to_deg <- function(rad) {
  return(rad * 180 / pi)
}

