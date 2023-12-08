#' Convert mass percentage to ppm
#'
#' Simple converter from wt% to ppm.
#'
#' @param wt numeric. mass percentage to be converted to ppm.
#'
#' @return numeric. ppm of given wt%
#'
#' @seealso Inverse conversion [ppm2wt()]
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
#' @param ppm numeric. concentration in ppm to be converted to wt%.
#'
#' @return numeric. wt% of given ppm
#'
#' @seealso Inverse conversion [wt2ppm()]
#'
#' @examples
#'   ppm2wt(10001)
#'   ppm2wt(10000.123:1000)
#'
#' @export
ppm2wt <- function(ppm) {
  ppm / 10000
}



#' Convert centimeter to inch
#'
#' Simple converter from centimeter to inch.
#'
#' @param cm numeric. Centimeter to be converted to inch
#'
#' @return numeric. Given centimeter in inch.
#'
#' @seealso Inverse conversion [in2cm()].
#'
#' @examples
#'   # One inch is defined as 25.4 millimeter
#'   cm2in(2.54)
#'
#'   # Works with a vector too
#'   cm2in(1:10)
#'
#' @export
cm2in <- function(cm) {
  cm/2.54
}



#' Convert inch to centimeter
#'
#' Simple converter from inch to centimeter.
#'
#' @param inch numeric. Inch to be converted to centimeter.
#'
#' @return numeric. Given inch in centimeter.
#'
#' @seealso Inverse conversion [cm2in()]
#'
#' @examples
#'   # One inch is defined as 25.4 millimeter
#'   in2cm(1)
#'
#'   # Works with a vector too
#'   in2cm(1:10)
#'
#' @export
in2cm <- function(inch) {
  inch*2.54
}
