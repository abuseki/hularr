% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/element_oxide.R
\name{ox2el}
\alias{ox2el}
\title{Convert oxide concentration to equivalent element concentration}
\usage{
ox2el(oxide, conc, LUT = elOxTable)
}
\arguments{
\item{oxide}{character. Formula of the oxide}

\item{conc}{numeric. Concentration of the oxide}

\item{LUT}{lookup table for the conversion factors -- must have the same data structure as \link{elOxTable} -- or \link{NULL}. If this is set to \code{NULL} \code{\link[=eloxFactor]{eloxFactor()}} will be used to calculate the conversion factor on the fly.}
}
\value{
numeric, Element concentration representing the given oxide concentration or \link{NA} if the given oxide could not be found in the lookup table (\link{elOxTable}).
}
\description{
Simple function to convert concentration of an oxides to its element
concentration using a lookup table (default is \link{elOxTable}) or using the \code{\link[=eloxFactor]{eloxFactor()}} if the lookup table is \link{NULL} or the given oxide could not be found in the lookup table.
}
\examples{
ox2el("Al2O3", 1)

}
\seealso{
\link{elOxTable}, \link{elOxTable_121} lookup tables for the conversion
}
