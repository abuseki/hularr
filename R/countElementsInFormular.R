#' Counts the elements a chemical formula
#'
#' Counts the elements occurring in a given chemical formula. The formula is
#' split into their elements using a regular expression.
#'
#' @param fmla Formula to count the elements therein as a character vector -- as
#'   string.
#'
#' @return A numeric vector with the found elements as names and their number of
#'   occurrence. This vector is ordered by the names of the elements
#'
#' @references The regular expression used herein was found on
#'   <https://regex101.com/r/vE6nC7/1>
#'
#' @examples
#'   countElementsInFormula('Mg2(SiO4)')
#'   countElementsInFormula('CaMgSi2O6')
#'   countElementsInFormula('Mg3Al2(SiO4)3')
#'   countElementsInFormula('NaMg3Al6(Si6O18)(BO3)3(OH)3(OH)')
#'
#' @export
countElementsInFormula <- function(fmla) {
  # message("parsing ", fmla, '...')

  # /([A-Z][a-z]?)(\d*(?:(?:[\.|\,])\d+(?:\%)?)?)|(?:[\(|\[])([^()]*(?:(?:[\(|\[]).*(?:[\)|\]]))?[^()]*)(?:[\)|\]])(\d*(?:(?:[\.|\,]?)\d+(?:\%)?))/g
  chem_regex <- "([A-Z][a-z]?)(\\d*(?:(?:[\\.|\\,])\\d+(?:\\%)?)?)|(?:[\\(|\\[])([^()]*(?:(?:[\\(|\\[]).*(?:[\\)|\\]]))?[^()]*)(?:[\\)|\\]])(\\d*(?:(?:[\\.|\\,]?)\\d+(?:\\%)?))"

  mm <- regmatches(fmla, gregexec(chem_regex, fmla, perl = TRUE))[[1]]

  # check for sub formulas
  subs <- which(mm[2,] == "")
  if(length(subs) != 0) {
    # handle sub formulas
    lsub <- unlist(lapply(subs, function(i) countElementsInFormula(mm[4, i]) * as.numeric(mm[5, i])))

    els <- as.numeric(mm[3,-subs])
    names(els) <- mm[2, -subs]

    els <- c(els, lsub)
  } else {
    # no sub formulas
    els <- as.numeric(mm[3,])
    names(els) <- mm[2,]
  }

  # replace NAs with one, e.g. until here in `TiO4` Ti would be `NA`
  els[which(is.na(els))] <- 1

  # sum up elements, since sub formulas can have the same elements
  l <- unlist(lapply(split(els, names(els)), sum))

  # guarrantee list is ordered my names of list elemenst
  l[order(names(l))]
}
