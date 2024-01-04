#' Table of Elements, oxides and their conversion factors
#'
#' A simple lookup table (LUT) of elements and their corresponding oxides, and
#' the conversion factor to convert a concentration given in oxide to an element
#' concentration or vice versa.\cr The oxides where built using the positive
#' oxidation states of an element as given in the periodic table of elements in
#' the georefdatar package ([georefdatar::pte]). The conversion factors were
#' then calculated using the standard atomic weights of the elements recommended
#' by the IUPAC and are quoted to five significant figures. So here are those
#' elements listed for those a standard atomic weight is given by the IUPAC.\cr
#' These factors are calculated and stored here for convenience and speed. They
#' could actually be calculated on the fly during a conversion.
#'
#' @details
#' * `elOxTable` holds the full of elements and oxides. There will be a
#' one-to-many relation between the elements, having more than one oxidation state, and their oxides -- e.g. Fe2+, Fe3+, Mn, Ti, U, ...
#' * `elOxTable_121` is a subset of `elOxTable` which holds only one (common) oxide for an element -- a one-to-one relationship between element and oxide. This is convenience and makes the use of [el2ox()] easier, since there is no need to pass an oxide to this function if one wants to limit the result to one.
#'
#'
#' @format A data frame with the following columns:\cr
#' \describe{
#' \item{Element}{character, Element's symbol}
#' \item{Oxide}{character, Element oxide. Depending on the possible oxidation states of an element, there can be a one-to-many relationship between the element and its oxides (`elOxTable`vs `elOxTable_121`). These are the polyvalent oxides e.g. Mn, V, U, ...}
#' \item{Factor}{numeric, Conversion factor to convert a concentration of an element to its oxide or vice versa.}
#' }
#'
#' @seealso
#' * [el2ox()], [ox2el()] for conversion functions
#' * [eloxFactor()] for the conversion factor
#' * [georefdatar::pte] for a list of oxidation states of an element
#' * [georefdatar::IUPAC_StdAW] and [georefdatar::aw()] for the standard
#'   atomic weights.
"elOxTable"

#' @rdname elOxTable
"elOxTable_121"



#' Convert element concentration to equivalent oxide concentration
#'
#' Simple function to convert concentration of an element to an oxide
#' concentration using a lookup table ([hularr::elOxTable]).
#'
#' @param element character. Symbol of the element
#' @param conc numeric. Concentration of the element
#' @param oxide character or [NULL]. Formula of the oxide the given element concentration shall be converted to. If this is `NULL` (default) the elements concentration will be converted to all found oxides in the lookup table ([hularr::elOxTable])
#' @param LUT lookup table for the conversion factors -- must have the same data structure as [elOxTable] -- or [NULL]. If this is set to `NULL` [eloxFactor()] will be used to calculate the conversion factor on the fly.
#'
#' @return named numeric. Oxide concentration(s) representing the given element's concentration or [NA]. If one oxide is explicitly given and found in the lookup table the named vector has a length of one. If none oxide is stated then the length is the number of oxides found for the element in the lookup table. If no oxide is found `NA` is returned.
#' @export
#'
#' @seealso [elOxTable], [elOxTable_121] lookup tables for the conversion
#'
#' @examples
#'
#' # Convert titanium explicitly to TiO2
#' el2ox("Ti", 1.2, "TiO2")
#'
#' # Convert to known oxides of uranium
#' el2ox("U", 0.2)
el2ox <- function(element, conc, oxide= NULL, LUT= elOxTable) {
  stopifnot("Don't know what to do! Either oxide or LUT or both must be given"= !is.null(oxide) | !is.null(LUT))

  # no LUT given do the work
  if (is.null(LUT))
    return(conc / eloxFactor(oxide))

  if (is.null(oxide))
    f <- LUT[
      LUT$Element == element,
      c("Oxide", "Factor")
    ]
  else
    f <- LUT[
      LUT$Element == element & LUT$Oxide == oxide,
      c("Oxide", "Factor")
    ]

  # not found in LUT, call our self without LUT
  if (nrow(f) == 0)
    return(el2ox(element, conc, oxide, LUT = NULL))


  stats::setNames(conc/f$Factor, f$Oxide)
}



#' Convert oxide concentration to equivalent element concentration
#'
#' Simple function to convert concentration of an oxides to its element
#' concentration using a lookup table (default is [hularr::elOxTable]) or using the [eloxFactor()] if the lookup table is [NULL] or the given oxide could not be found in the lookup table.
#'
#' @param oxide character. Formula of the oxide
#' @param conc numeric. Concentration of the oxide
#' @param LUT lookup table for the conversion factors -- must have the same data structure as [elOxTable] -- or [NULL]. If this is set to `NULL` [eloxFactor()] will be used to calculate the conversion factor on the fly.
#'
#' @return numeric, Element concentration representing the given oxide concentration or [NA] if the given oxide could not be found in the lookup table ([hularr::elOxTable]).
#' @export
#'
#' @seealso [elOxTable], [elOxTable_121] lookup tables for the conversion
#'
#' @examples
#' ox2el("Al2O3", 1)
#'
ox2el <- function(oxide, conc, LUT= elOxTable) {
  if (is.null(LUT)) {
    f <- eloxFactor(oxide)
  } else {
    f <- LUT[LUT$Oxide == oxide, "Factor"]
    if (length(f) == 0)
      f <- eloxFactor(oxide)
  }

  conc * f
}



#' Element to stoichiometric oxide conversion factor
#'
#' A general function to calculate the factor needed to convert an element
#' concentration to it's stoichiometric oxide or vice versa.\cr
#' To convert an oxide concentration to it's element concentration multiply by this factor. To convert an element concentration to the equivalent expressed as oxide divide by this factor.
#'
#'
#' @param oxide character. The chemical formula of an oxide the conversion factor shall be calculated for
#'
#' @return numeric. The conversion factor.
#' @export
#'
#' @seealso
#' * [el2ox()], [ox2el()] for conversion functions
#' * [elOxTable], [elOxTable_121] lookup tables for the conversation
#' * [mass()] to calculate the molecular mass
#' * [georefdatar::IUPAC_StdAW] and [georefdatar::aw()] for the standard
#'   atomic weights.
#' * [georefdatar::pte] for a list of oxidation states of an element
#'
#' @examples
#' # convert 0.25 lithium (%wt) to it's equivalent expressed as Li2O
#' 0.25 / eloxFactor("Li2O")
#'
eloxFactor <- function(oxide) {
  neif <- hularr::countElementsInFormula(oxide)
  idEl <- which(names(neif) != 'O')
  idOx <- which(names(neif) == 'O')
  elSy <- names(neif)[idEl]
  signif(georefdatar::aw(elSy) * neif[idEl] / hularr::mass(oxide), 5)
}

