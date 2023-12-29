#' Calculate molecular mass
#'
#' Calculates the relative molecular mass aka molecular weight of a given
#' chemical formula.
#'
#' @param fmla character. The chemical formula for which to calculate mass.
#' @param aws numeric. Named vector with atomic weights. Names represent the
#'   symbols of elements, and their values are their atomic weights. By default
#'   this refers to the IUPAC standard atomic weights table of the georefdatar
#'   package.
#'
#'
#' @return A numeric value representing the molecular mass of the formula.
#'
#' @seealso georefdatar IUPAC_StdAW standard atomic weights of the elements
#'   as recommended by the IUPAC.
#'
#' @importFrom stats setNames
#'
#' @export
#'
#' @examples
#'   # Water
#'   mass("H2O")
#'
#'   # antigorite
#'   mass("Mg3Si2O5(OH)4")
#'
#'   # muscovite
#'   mass("KAl2(Si3Al)O10(OH)2")
#'
mass <- function(fmla,
                 aws= setNames(georefdatar::IUPAC_StdAW$`abrStdAW::Value`,
                              georefdatar::IUPAC_StdAW$Symbol)) {
  # count elements in formular
  neif <- hularr::countElementsInFormula(fmla)

  # calcuate mass of formular
  sum(aws[names(neif)] * neif)
}

