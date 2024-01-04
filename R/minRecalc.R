#' Mineral Formula Recalculation
#'
#' Simple recalculation element oxides -- e.g. from of electron microprobe analyses (EPMA) --  to atoms per formula unit (apfu) on the basis of fixed number of oxygen apfu.\cr
#' This is a very basic function for mineral formula recalculation. There is no site distribution nor (end)member classification done here.
#'
#' @param ox_wt list of element oxides and their weight percents (%wt)
#' @param ox_pfu number of oxygen atoms per formula.
#'
#' @return a list with the following entries:\cr
#' \describe{
#' \item{recalc}{data.frame, showing the calculation. It has the following attributes (columns):
#'   \describe{
#'   \item{Oxide}{the given oxides (`ox_wt`)}
#'   \item{mw}{Used molecular weight of the oxides}
#'   \item{wt}{Weight percentages (%wt) of the oxides}
#'   \item{MoleUnits}{_mole number_, weight percentage per molecular weight, `wt/mw`}
#'   \item{OxygenUnits}{_oxygen number_, `MoleUnits` times the number of oxygen atoms in this oxide}
#'   \item{NormalizedOxygenUnits}{`OxygenUnits * (ox_pfu/totalOxUnits)`, where totalOxUnits is the sum of all the `OxygenUnits`}
#'   \item{AtomUnits}{`NormalizedOxygenUnits` times the ratio of cations and oxygen atoms in the oxide}
#'   \item{Cation}{Name/Symbol of the cation in the given oxide }
#'   }
#' }
#' \item{sum_ox_wt}{numeric, sum of the given element oxides}
#' \item{sum_ox_atms}{numeric, number of oxygen atoms in the formula}
#' \item{sum_cat_atms}{numeric, number of cation atoms in the formula}
#' }

#' @export
#'
#' @examples
#' # Examples from https://sarahlambart.com/teaching/mineralogy-06-01.pdf
#' # Feldspar
#' fsp <- c(SiO2= 68.20, Al2O3=19.19, Na2O=10.20, K2O=2.32)
#' minRecalc(fsp, 8)
#' # Shows a feldspar with formula (Na_0.87, K_0.13)AlSi_3O_8
#'
#' # Pyroxene
#' px <- c(SiO2= 56.64, Na2O= 4.38, Al2O3= 7.21, MgO= 13.30, CaO= 18.46)
#' mrc <- minRecalc(px, 6)
#' cbind(round(mrc$recalc$AtomUnits, 1), mrc$recalc$Cation)
#' # Shows a pyroxene with formula (Na_0.3, Ca_0.7, Mg_0.7, Al_0.3)Si_2O_6
#'
#' # Obligatory garnet, this analysis is taken from
#' # (Gregor Markl, Minerale und Gesteine, 2015, Springer)
#' grt <- c(SiO2= 37.30 , Al2O3= 21.10, FeO= 29.90,  MnO= 1.14, MgO= 0.9, CaO=9.02)
#' mrc <- minRecalc(grt, 12)
#'
#' # list of elements an their apfu
#' x <- split(round(mrc$recalc$AtomUnits, 2), mrc$recalc$Cation)
#' x
#' # Shows a garnet with the formula:
#' # Fe_2.01 Ca_0.78 Mg_0.11 Mn_0.08 Al_2 Si_3O8
#'
#' # endmembers
#' ## only consider the bivalent site
#' bival <- sum(x$Ca, x$Fe, x$Mg, x$Mn)
#' ## calculate endmembers
#' xs<- c(xAlm= x$Fe / bival, xPrp= x$Mg / bival, xSps= x$Mn / bival, xGrs= x$Ca / bival)
#' round(xs, 2)
#'
minRecalc <- function(ox_wt, ox_pfu) {
  # Names resp. chem. formulas of oxides
  ox_names <- names(ox_wt)

  # Count elements in oxide formulas
  fmlas <- lapply(ox_names, hularr::countElementsInFormula)
  # names/symbol of cations in oxide formulas
  cat_names <- names(unlist(lapply(fmlas, function(x) x[! names(x) %in% "O"])))
  # number of cations in oxide formulas
  ox_ncat <- unlist(lapply(fmlas, function(x) x[! names(x) %in% "O"]))
  # number of ox atms in oxide formulas
  ox_nox <- unlist(lapply(fmlas, function(x) x[names(x) %in% "O"]))

  # formula weight of oxides
  ox_mw <-sapply(ox_names, hularr::mass)

  # mole number
  moleUnitsOxide <- ox_wt/ox_mw

  # oxygen number
  oxUnits <- moleUnitsOxide*ox_nox
  totalOxUnits <- sum(oxUnits, na.rm = TRUE)

  # normalized oxygen number
  normOxUnits <- oxUnits * ox_pfu/totalOxUnits

  # cations atom units
  cat_atomUnits <- normOxUnits* (ox_ncat/ox_nox)

  list(
    recalc=  data.frame(
      Oxide= ox_names,
      mw= ox_mw,
      wt= ox_wt,
      MoleUnits= moleUnitsOxide,
      OxygenUnits= oxUnits,
      NormalizedOxygenUnits= normOxUnits,
      AtomUnits= cat_atomUnits,
      Cation= cat_names
    ),
    sum_ox_wt= sum(ox_wt, na.rm = TRUE),
    sum_ox_atms= sum(normOxUnits, na.rm = TRUE),
    sum_cat_atms= sum(cat_atomUnits, na.rm = TRUE)
  )
}
