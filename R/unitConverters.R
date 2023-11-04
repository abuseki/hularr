#' Convert mass percentage to ppm
#'
#' Simple converter from wt% to ppm.
#'
#' @param wt mass percentage (wt%) to be converted to ppm as a numeric vector.
#'
#' @return numeric vector of converted wt%
#'
#' @examples
#'   wt2ppm(56.6)
#'   wt2ppm(3.5:6.5)
#'
#' @export
wt2ppm <- function(wt) {
  wt * 10000
}



#' Convert ppm to mass percentage
#'
#' Simple converter from ppm to wt%
#'
#' @param ppm concentration in ppm to be converted to mass percentage (wt%) as a numeric vector
#'
#' @return numeric vector of converted ppm
#'
#' @examples
#'   ppm2wt(10001)
#'   ppm2wt(10000.123:1000)
#'
#' @export
ppm2wt <- function(ppm) {
  ppm / 10000
}
