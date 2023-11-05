#' Compute the area of a polygon
#'
#' Calculates the area of simple polygons using the well-known formula:
#' \deqn{
#'    2A=\sum_{i=0}^{n-1}x_i \ y_{i+1} \ - \ x_{i+1} \ y_i
#' }
#'
#' @param p Matrix-shaped data structure with two columns representing the x
#'   and y values of the polygon vertices. The polygon must be closed. When used
#'   in conjunction with obtaining the convex hull of a point set using
#'   functions such as [grDevices::chull()], the first point must be added at the end to
#'   ensure that the polygon is closed.
#'
#' @return The area of the polygon
#'
#' @examples
#' # Simple
#' polyarea(matrix(c(0,2,2,0, 0,0,2,2), ncol=2))
#'
#' # more complex
#' p <- matrix(rgamma(100, 20, 1), ncol = 2)
#' # get the convex hull
#' p <- p[chull(p),]
#' # add 2st point, so ploygon is closed
#' p <- rbind(p, p[1,])
#' # Finally get the area
#' polyarea(p)
#'
#' @export
polyarea <- function(p) {
    x <- p[,1]
    y <- p[,2]
    x_ <- c(x[-1], x[1])
    y_ <- c(y[-1], y[1])

    abs(sum(x*y_ - x_*y)/2)
}
