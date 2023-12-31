% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mass.R
\name{mass}
\alias{mass}
\title{Calculate molecular mass}
\usage{
mass(
  fmla,
  aws = setNames(georefdatar::IUPAC_StdAW$`abrStdAW::Value`,
    georefdatar::IUPAC_StdAW$Symbol)
)
}
\arguments{
\item{fmla}{character. The chemical formula for which to calculate mass.}

\item{aws}{numeric. Named vector with atomic weights. Names represent the
symbols of elements, and their values are their atomic weights. By default
this refers to the IUPAC standard atomic weights table of the georefdatar
package.}
}
\value{
A numeric value representing the molecular mass of the formula.
}
\description{
Calculates the relative molecular mass aka molecular weight of a given
chemical formula.
}
\examples{
# Water
mass("H2O")

# antigorite
mass("Mg3Si2O5(OH)4")

# muscovite
mass("KAl2(Si3Al)O10(OH)2")

# Si wt\% in muscovite
round(mass("Si3")/mass("KAl2(Si3Al)O10(OH)2") *100, 2)

}
\seealso{
georefdatar IUPAC_StdAW standard atomic weights of the elements
as recommended by the IUPAC.
}
