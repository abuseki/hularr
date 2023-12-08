#' How many pixels to for cm/inch to give specific dots per inch (dpi).
#'
#' Calculate the number of pixels needed for an image or a figure to be of
#' specified size in centimeters or inches (e.g. width or height) at a specified
#' number of dots per inch (dpi).
#'
#' @param size numeric. Target size in cm or inches
#' @param unit Unit of `size`. Either centimeter ("cm") or inch ("inch"). Given
#'   centimeters will be calculated to inches if needed.
#' @param dpi  numeric. Target dots per inch.
#'
#' @return numeric. Required pixel for a figure to be at the given size when
#'   printed at the given dpi.
#'
#' @examples
#' # We want to print a figure on A4 paper (21cm wide) with 2.5cm margins on the
#' # left and right sides at 300dpi. How many pixels does the figure need?
#' pixel4dpi(21-2*2.5)
#' # rounded since there are no sub pixels
#' round(pixel4dpi(21-2*2.5))
#'
#' # Same question but calculate the pixel for multiple dpi.
#' pixel4dpi(21-2*2.5, dpi = c(150, 300, 600))
#'
#' @export
pixel4dpi <- function(size, unit="cm", dpi= 300) {
  stopifnot("Unit must be 'cm' or 'inch'"= unit %in% c("cm", "inch"))
  if (unit == "cm")
    size <- cm2in(size)

  dpi * size
}
