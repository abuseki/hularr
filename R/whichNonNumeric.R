#' Which indices are non numeric?
#'
#' Finds non-numeric values in a vector-like data structure and can easily be
#' applied to table-like data structures.\cr This is useful for data cleaning to
#' find the "wrong" entries, where all entries should be numeric but are not.
#' NAs are not affected, they are treated as numeric values.
#'
#'
#' @param x A vector where to look for the non numeric values. NAs are not
#'   affected.
#'
#' @return  Indices of the non numeric entries in `x`.
#'
#' @examples
#' # basic usage
#' t <- c(1:5, 'X', 7:10, ' ', 12:15, 'bad')
#' # get indices of "bad" values
#' which.nonnumeric(t)
#' # show them
#' t[which.nonnumeric(t)]
#'
#' # more realistic find the erroneous entries in some measurements
#' t <- data.frame(
#'   sampleName= c(LETTERS[1:10]),
#'   V1= c(1, 'bad', 3:10),
#'   V2= c(1:2, 'XXX', 4:10),
#'   V3= c(1:8, NA, ' ')
#' )
#'
#' # get only the indices
#' unlist(lapply(t[-1], which.nonnumeric))
#'
#' # resolve the rows having the erroneous entries
#' t[unlist(lapply(t[-1], which.nonnumeric)), ]
#'
#' @export
which.nonnumeric <- function(x) {

  # find the non numeric by conversion
  nonums <- is.na(suppressWarnings(as.numeric(as.character(x))))

  # filter non numeric and those that where NA before upper conversion
  which(nonums & !is.na(x))
}


