#' Find the Euclidean distance between two points
#'
#' @description [distance()] is a function to calculate distance between two points.
#' This is a helper function for [aniVRips()] and [staticVRips()]
#' 
#' @param x1,y1,x2,y2 coordinate point values.
#'
#' @return a numeric distance
#' @export
#'
distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}