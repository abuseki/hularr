#' Euclidean distance between two points
#'
#' Calculate the Euclidean distance between two points. The Euclidean distance
#' is the distance between two points if they are connected by a straight line.
#' It is the square root of the sum of differences between two points.
#'
#' @param X numeric vector. Coordinates of one point.
#' @param Y numeric vector. Coordinates of the other point.
#'
#' @return numeric scalar. The euclidean distance between the two given points.
#'
#' @export
#'
#' @examples
#' # Length of straight line in two dimensions -- equal `sqrt(2)`
#' dist.euclidean(c(0, 0), c(1, 1))
#'
#' # Length of straight line in three dimensions -- equal `sqrt(3)`
#' dist.euclidean(c(0, 0, 0), c(1, 1, 1))
#'
dist.euclidean <- function(X, Y) {
  sqrt(sum((X - Y) ^ 2))
}



#' Manhattan distance between two points
#'
#' Calculate the Manhattan distance also known as the block distance or taxicab
#' distance between two points. It is the sum of absolute differences of the
#' (Cartesian) coordinates of two different points.
#'
#' @param X numeric vector. Coordinates of one point.
#' @param Y numeric vector. Coordinates of the other point.
#'
#' @return numeric scalar. The Manhattan distance between the two given points.
#'
#' @export
#'
#' @examples
#' # Distance to walk on the diagonal corner of the unit block
#' dist.manhattan(c(0, 0), c(1, 1))
#'
#' # Distance to walk on the diagonal corner of the unit block and one floor up
#' dist.manhattan(c(0, 0, 0), c(1, 1, 1))
#'
dist.manhattan <- function(X, Y) {
  sum(abs(X-Y))
}



#' Chebyshev distance between two vectors
#'
#' The Chebyshev distance is the maximum of the absolute differences of the
#' coordinates of two vectors.
#'
#' @param X numeric vector. Coordinates of one point.
#' @param Y numeric vector. Coordinates of the other point.
#'
#' @return numeric scalar. The Chebyshev distance between the two vectors.
#'
#' @export
#'
#' @examples
#' # Classic example: How many moves must a king make on a chessboard to get
#' # from e.g. `f3` (6, 3) to `b8` (2, 8)?
#' dist.chebyshev(c(6, 3), c(2, 8))
#'
dist.chebyshev <- function(X, Y) {
  max(abs(X-Y))
}
