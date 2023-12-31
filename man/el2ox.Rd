% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/element_oxide.R
\name{el2ox}
\alias{el2ox}
\title{Convert element concentration to equivalent oxide concentration}
\usage{
el2ox(element, conc, oxide = NULL, LUT = elOxTable)
}
\arguments{
\item{element}{character. Symbol of the element}

\item{conc}{numeric. Concentration of the element}

\item{oxide}{character or \link{NULL}. Formula of the oxide the given element concentration shall be converted to. If this is \code{NULL} (default) the elements concentration will be converted to all found oxides in the lookup table (\link{elOxTable})}

\item{LUT}{lookup table for the conversion factors -- must have the same data structure as \link{elOxTable} -- or \link{NULL}. If this is set to \code{NULL} \code{\link[=eloxFactor]{eloxFactor()}} will be used to calculate the conversion factor on the fly.}
}
\value{
named numeric. Oxide concentration(s) representing the given element's concentration or \link{NA}. If one oxide is explicitly given and found in the lookup table the named vector has a length of one. If none oxide is stated then the length is the number of oxides found for the element in the lookup table. If no oxide is found \code{NA} is returned.
}
\description{
Simple function to convert concentration of an element to an oxide
concentration using a lookup table (\link{elOxTable}).
}
\examples{

# Convert titanium explicitly to TiO2
el2ox("Ti", 1.2, "TiO2")

# Convert to known oxides of uranium
el2ox("U", 0.2)
}
\seealso{
\link{elOxTable}, \link{elOxTable_121} lookup tables for the conversion
}
